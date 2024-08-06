import warnings
from tqdm import TqdmExperimentalWarning, tqdm
from beir import util, LoggingHandler
from beir.datasets.data_loader import GenericDataLoader
import logging
from beir.retrieval.search.lexical import BM25Search as BM25
from beir.retrieval.evaluation import EvaluateRetrieval
import psycopg
import os
from dotenv import load_dotenv

load_dotenv()

ELASTICSEARCH_HOST = "localhost"
POSTGRES_URL = os.getenv(
    "POSTGRES_URL") or "postgres://postgres:postgres@localhost:5432/postgres"
K_VALUES = [1, 3, 5, 10, 100, 1000]

# Configure logging to print debug information to stdout
warnings.filterwarnings("ignore", category=TqdmExperimentalWarning)
logging.basicConfig(format='%(asctime)s - %(message)s',
                    datefmt='%Y-%m-%d %H:%M:%S',
                    level=logging.INFO,
                    handlers=[LoggingHandler()])


def load_dataset(dataset: str, out_dir='datasets') -> tuple:
    """
    Load the specified dataset from the BEIR repository.

    Args:
        dataset (str): The name of the dataset to load.
        out_dir (str): The directory to unzip the dataset into.

    Returns:
        tuple: A tuple containing the corpus, queries, and qrels (ground truth relevance judgments).
    """

    # Download the dataset from the specified URL and unzip it into the specified directory
    url = f"https://public.ukp.informatik.tu-darmstadt.de/thakur/BEIR/datasets/{dataset}.zip"
    data_path = util.download_and_unzip(url, out_dir)

    # Load the dataset from the specified data_path for the test split
    corpus, queries, qrels = GenericDataLoader(
        data_folder=data_path).load(split="test")

    # 'corpus' is a dictionary where keys are document IDs and values are documents.
    # 'queries' is a dictionary where keys are query IDs and values are query texts.
    # 'qrels' (query relevance judgments) is a dictionary that maps query IDs to a dictionary of relevant document IDs and their relevance scores.
    return corpus, queries, qrels


def evaluate_elasticsearch(dataset: str, corpus: dict, queries: dict, qrels: dict) -> dict:
    """
    Run and evaluate Elasticsearch retrieval on the specified dataset.

    Args:
        dataset (str): The name of the dataset that was loaded
        corpus (dict): A dictionary where keys are document IDs and values are documents.
        queries (dict): A dictionary where keys are query IDs and values are query texts.
        qrels (dict): A dictionary that maps query IDs to a dictionary of relevant document IDs and their relevance scores.

    Returns:
        dict: A dictionary containing the NDCG, MAP, Recall, and Precision scores for the retrieval results.
    """

    model = BM25(index_name=dataset + "_index",
                 hostname=ELASTICSEARCH_HOST, initialize=True)
    retriever = EvaluateRetrieval(model)
    results = retriever.retrieve(corpus, queries)
    ndcg, _map, recall, precision = retriever.evaluate(
        qrels, results, K_VALUES)
    return {"NDCG": ndcg, "MAP": _map, "Recall": recall, "Precision": precision}


def setup_tables(dataset: str, corpus: dict, queries: dict) -> None:
    """
    Set up the tables for the specified dataset.

    Args:
        dataset (str): The name of the dataset that was loaded
        corpus (dict): A dictionary where keys are document IDs and values are documents.
    """
    with psycopg.connect(POSTGRES_URL, cursor_factory=psycopg.ClientCursor) as conn:
        with conn.cursor() as cur:
            cur.execute(f"""
                SELECT EXISTS (
                    SELECT 1 
                    FROM information_schema.tables 
                    WHERE table_schema = 'public' 
                    AND (
                        table_name = '{dataset}'
                        OR table_name = '{dataset}_query'
                    )
                )
            """)
            if cur.fetchone()[0]:
                return

            # Create tables for the dataset and queries
            cur.execute(
                f"CREATE TABLE {dataset} (id TEXT PRIMARY KEY, document TEXT, sparse_vector sparsevec(30522), dense_vector vector(768))")
            cur.execute(
                f"CREATE TABLE {dataset}_query (id TEXT PRIMARY KEY, document TEXT, sparse_vector sparsevec(30522), dense_vector vector(768))")

            # Insert IDs and texts into the database in batches
            BATCH_SIZE = 5000
            data = [(id, document['text']) for id, document in corpus.items()]
            for i in tqdm(range(0, len(data), BATCH_SIZE)):
                batch_data = data[i:i + BATCH_SIZE]
                values_str = ', '.join(cur.mogrify("(%s, %s)", row)
                                       for row in batch_data)
                cur.execute(
                    f"INSERT INTO {dataset} (id, document) VALUES {values_str}")

            # Insert IDs and texts into the query table
            data = [(id, query) for id, query in queries.items()]
            values_str = ', '.join(cur.mogrify("(%s, %s)", row)
                                   for row in data)
            cur.execute(
                f"INSERT INTO {dataset}_query (id, document) VALUES {values_str}")


def setup_indexes(dataset: str, only='') -> None:
    """
    Set up the indexes for the specified dataset.
    NOTE: This is done after the embedding jobs are setup.

    Args:
        dataset (str): The name of the dataset that was loaded
    """
    with psycopg.connect(POSTGRES_URL) as conn:
        with conn.cursor() as cur:
            if only != "sparse":
                cur.execute(f"""
                    SET maintenance_work_mem = '2GB';
                    CREATE INDEX ON {dataset} USING hnsw(dense_vector vector_cosine_ops) WITH (m=16, ef_construction=128);
                """)
            if only != "dense":
                cur.execute(f"""
                    SET maintenance_work_mem = '2GB';
                    CREATE INDEX ON {dataset} USING hnsw(sparse_vector sparsevec_cosine_ops) WITH (m=16, ef_construction=128);
                """)


def evaluate_hybrid_vector_search(dataset: str, qrels: dict) -> dict:
    """
    Run and evaluate dense retrieval on the specified dataset.

    Args:
        dataset (str): The name of the dataset that was loaded
        qrels (dict): A dictionary that maps query IDs to a dictionary of relevant document IDs and their relevance scores.

    Returns:
        dict: A dictionary containing the NDCG, MAP, Recall, and Precision scores for the retrieval results.
    """

    with psycopg.connect(POSTGRES_URL) as conn:
        with conn.cursor() as cur:
            cur.execute(
                f"SELECT id, sparse_vector, dense_vector FROM {dataset}_query")
            query_data = cur.fetchall()

            results = {}
            for query_id, sparse_vector, dense_vector in query_data:
                result = {}

                cur.execute(f"""
                    SELECT
                        id
                    FROM {dataset}
                    ORDER BY
                        sparse_vector <=> %s
                    LIMIT 1000
                """, (sparse_vector,))
                top_k_sparse = cur.fetchall()

                cur.execute(f"""
                    SELECT
                        id
                    FROM {dataset}
                    ORDER BY
                        dense_vector <=> %s
                    LIMIT 1000
                """, (dense_vector,))
                top_k_dense = cur.fetchall()

                top_ids = list(set([match_id for match_id, in top_k_sparse] +
                                   [match_id for match_id, in top_k_dense]))
                if isinstance(top_ids[0], int):
                    top_ids_str = ','.join(map(str, top_ids))
                else:
                    top_ids_str = ','.join(f"'{str(id)}'" for id in top_ids)

                cur.execute(f"""
                    SELECT
                        id,
                        0.2 * (sparse_vector <=> %s) + 0.8 * (dense_vector <=> %s) AS distance
                    FROM {dataset}
                    WHERE id IN ({top_ids_str})
                    ORDER BY
                        2
                """, (sparse_vector, dense_vector))
                top_k = cur.fetchall()
                for match_id, distance in top_k:
                    result[str(match_id)] = -distance
                results[str(query_id)] = result

            retriever = EvaluateRetrieval()
            logging.info("Retriever evaluation for k in: {}".format(K_VALUES))
            ndcg, _map, recall, precision = retriever.evaluate(
                qrels, results, K_VALUES)
            return {"NDCG": ndcg, "MAP": _map, "Recall": recall, "Precision": precision}


def evaluate_sparse_vector_search(dataset: str, qrels: dict) -> dict:
    """
    Run and evaluate sparse retrieval on the specified dataset.

    Args:
        dataset (str): The name of the dataset that was loaded
        qrels (dict): A dictionary that maps query IDs to a dictionary of relevant document IDs and their relevance scores.
    """

    with psycopg.connect(POSTGRES_URL) as conn:
        with conn.cursor() as cur:
            cur.execute(f"SELECT id, sparse_vector FROM {dataset}_query")
            query_data = cur.fetchall()

            results = {}
            for query_id, sparse_vector in tqdm(query_data, desc="Processing queries"):
                result = {}
                cur.execute(f"""
                    SELECT
                        id,
                        sparse_vector <=> %s AS distance
                    FROM {dataset}
                    ORDER BY
                        sparse_vector <=> %s
                    LIMIT 1000
                """, (sparse_vector, sparse_vector))
                top_k = cur.fetchall()
                for match_id, distance in top_k:
                    result[str(match_id)] = -distance
                results[str(query_id)] = result

            retriever = EvaluateRetrieval()
            logging.info("Retriever evaluation for k in: {}".format(K_VALUES))
            ndcg, _map, recall, precision = retriever.evaluate(
                qrels, results, K_VALUES)
            return {"NDCG": ndcg, "MAP": _map, "Recall": recall, "Precision": precision}


def evaluate_dense_vector_search(dataset: str, qrels: dict) -> dict:
    """
    Run and evaluate dense retrieval on the specified dataset.

    Args:
        dataset (str): The name of the dataset that was loaded
        qrels (dict): A dictionary that maps query IDs to a dictionary of relevant document IDs and their relevance scores.        
    """

    with psycopg.connect(POSTGRES_URL) as conn:
        with conn.cursor() as cur:
            cur.execute(f"SELECT id, dense_vector FROM {dataset}_query")
            query_data = cur.fetchall()

            results = {}
            for query_id, dense_vector in query_data:
                result = {}
                cur.execute(f"""
                    SELECT
                        id,
                        dense_vector <=> %s AS distance
                    FROM {dataset}
                    ORDER BY
                        dense_vector <=> %s
                    LIMIT 1000
                """, (dense_vector, dense_vector))
                top_k = cur.fetchall()
                for match_id, distance in top_k:
                    result[str(match_id)] = -distance
                results[str(query_id)] = result

            retriever = EvaluateRetrieval()
            logging.info("Retriever evaluation for k in: {}".format(K_VALUES))
            ndcg, _map, recall, precision = retriever.evaluate(
                qrels, results, K_VALUES)
            return {"NDCG": ndcg, "MAP": _map, "Recall": recall, "Precision": precision}
