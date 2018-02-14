from glob import glob
from pathlib import Path

from invoke import task, Collection
from unipath import Path
import jinja2

import graphdb

from tasks.config import PROJ


PROJ = Path(__file__).absolute().parent
R_PKG = Path(PROJ, 'data')
REPORTS = Path(PROJ, 'reports')
TOTEMS = Path(PROJ, 'experiment')
ITEM_IMAGES = Path(R_PKG, 'inst/extdata/items')
TOTEMS_RAW_DATA = Path(R_PKG, 'data-raw/totems')


@task
def configure(ctx):
    """Create environment file from a template."""
    dst = '.environment'
    template = jinja2.Template(open('environment.j2', 'r').read())
    with open(dst, 'w') as f:
        f.write(template.render())

@task
def load(ctx):
    """Load the Neo4j graph db with totems data."""
    graphdb.load(delete_first=True)

@task
def install(ctx):
    """Install the totems R package."""
    ctx.run('cd {} && Rscript -e "devtools::install()"'.format(R_PKG))

@task
def make(ctx, name, clear_cache=False, open_after=False, verbose=False,
         output_format="bookdown::pdf_document2"):
    """Compile RMarkdown documents.

    Examples:

      $ inv make list      # see available reports
      $ inv make all       # run all reports
      $ inv make {stem} -o # make {stem}.Rmd and open after

    """
    if name == 'list':
        print('Available docs:')
        available_docs = get_available_docs()
        for rmd in available_docs:
            print(' - %s' % rmd.stem)
        sys.exit()

    docs = get_available_docs(name)
    failed = []

    cmd = 'Rscript -e "rmarkdown::render({!r}, output_format={!r})"'
    for doc in docs:
        if clear_cache:
            clean(ctx, doc, verbose=verbose)

        result = ctx.run(cmd.format(str(doc), output_format), echo=verbose, warn=True)

        if not result.ok:
            failed.append(str(doc))

        if open_after and result.ok:
            output_file = Path(doc.parent, '{}.pdf'.format(doc.stem))
            ctx.run('open {}'.format(output_file), echo=verbose)

    if failed:
        print('The following docs had errors:')
        for doc in failed:
            print(' - {}'.format(doc))

@task
def clean(ctx, name, verbose=False):
    """Clean the cache and intermediate outputs of RMarkdown reports."""
    if name == 'all':
        parent = f'{PROJ}/docs/'
        stem = '*'
    else:
        doc = get_available_docs(name)[0]
        parent = doc.parent
        stem = doc.stem

    cmd = ('cd {parent} && rm -rf {stem}_cache/ {stem}_files/ '
           'code* '
           '{stem}.pdf {stem}.docx {stem}.html {stem}.md {stem}.tex '
           '{stem}.aux {stem}.out {stem}.log {stem}.synctex.gz {stem}.bbl')
    ctx.run(cmd.format(parent=parent, stem=stem), echo=verbose)


def get_available_docs(name=''):
    available_docs = [Path(rmd) for rmd in
                      glob('{proj}/docs/**/*.Rmd'.format(proj=PROJ),
                           recursive=True)
                      if Path(rmd).is_file()]

    if name == '':
        rmds = available_docs
    elif Path(name).is_file():
        rmds = [Path(name)]
    else:
        # name is a glob
        rmds = [Path(rmd) for rmd in
                glob('{proj}/docs/**/{name}*.Rmd'.format(proj=PROJ, name=name),
                     recursive=True)
                if Path(rmd).is_file()]

    return rmds
