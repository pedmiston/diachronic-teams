from invoke import task
from unipath import Path
import webbrowser


download_opts = dict(
    username='Your Kaggle username',
    password='Your Kaggle password',
)

@task(help=download_opts)
def download(ctx, username=None, password=None):
    """Download the public Kaggle competition sqlite database.

    If username and password are not provided, they will be searched for
    in the bash environment under the names KAGGLE_USERNAME and
    KAGGLE_PASSWORD.

    References:
        https://www.kaggle.com/forums/f/809/product-feedback/t/21591/kaggle-api-for-data-download-and-submission/123361#post123361
    """
    downloads_url = "https://www.kaggle.com/kaggle/meta-kaggle/downloads/{}"
    filename = "database.sqlite.zip"

    # Attempt to download the file (get)
    r = requests.get(downloads_url.format(filename))

    # Login (post)
    # If didn't get through, login with a post
        username = username or environ.get('KAGGLE_USERNAME')
        password = password or environ.get('KAGGLE_PASSWORD')
        if not username or not password:
            # descriptive error
            pass

        creds = requests.post()

        r = requests.get(downloads_url.format(filename), creds=creds)
        with open(dst, 'wb') as out:
            for chunk in r.iter_content(CHUNK_SIZE):
                out.write(chunk)
