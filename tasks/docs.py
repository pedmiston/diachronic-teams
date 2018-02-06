from glob import glob
from pathlib import Path

from invoke import task

from tasks.config import PROJ

@task
def make(ctx, name, clear_cache=False, open_after=False, verbose=False, output_format="pdf_document"):
    """Compile RMarkdown documents.

    Examples:

      $ inv make list      # see available reports
      $ inv make all       # run all reports
      $ inv make totems -o # make totems.Rmd and open output after

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
    docs = get_available_docs(name)

    for doc in docs:
        ctx.run((f'cd {doc.parent} && rm -rf '
                  '*_cache/ *_files/ '
                  'code* '
                  '*.pdf *.docx *.html *.md '
                  '*.tex *.aux *.out *.log *.synctex.gz *.bbl'),
                  echo=verbose)

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
