from invoke import task
from unipath import Path
import webbrowser


ROOT = Path(__file__).absolute().ancestor(2)
R_PKG = Path(ROOT, 'evoteams')
DST = Path(R_PKG, 'data-raw/kaggle')

if not DST.isdir():
    DST.mkdir(True)


@task
def download(ctx):
    """Download the public Kaggle competition sqlite database."""
    downloads_url = "https://www.kaggle.com/kaggle/meta-kaggle/downloads/{}"
    filename = "database.sqlite.zip"
    webbrowser.open(downloads_url.format(filename))
    print("When the download completes, unzip it and move it to the R pkg:\n"
          "\t$ unzip ~/Downloads/database.sqlite.zip -d evoteams/data-raw/kaggle")
