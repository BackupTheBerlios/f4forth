/* f3view.c
**
** fast (and dirty) program for printing FORTH screen files to stdout
**
*/
/* hp'2.10.02, F4: lines=blocks, blocks over all*/
#include <stdio.h>
#include <stdlib.h>
#include <unistd.h>
#include <string.h>
#include <errno.h>
#include <sysexits.h>

#define LINESZ 64
#define NLINES 16
#if 1
#define SPECIALS "\f"
#else
#define SPECIALS ""
#endif

void print_screenfile (FILE *fp, char *file)
{
    char line[LINESZ+1];
    int sc = 0, lc = 0;
    ssize_t rsz;
    while ((rsz = fread (line, 1, LINESZ, fp)) > 0) {
	line[rsz] = '\0';
	if (lc == 0) {
	    if (sc > 0 && sc % 3 == 0) { printf ("%s", SPECIALS); }
	    printf ("\n\nSCR #%-5d (%s):\n\n", sc++, file);
	}
	printf ("%3d: %s :%5d\n", lc /*+ 1*/, line, (sc-1)*16+lc/**/); lc = (lc + 1) % NLINES;
    }
    while (lc != 0) { printf ("%3d:~~~\n", lc); lc = (lc + 1) % NLINES; }
    printf ("\n\n%s", SPECIALS);
}

static char *progname;

int main (int argc, char *argv[])
{
    char *file;
    FILE *fp;
    int ix;
    progname = strrchr (argv[0], '/');
    if (progname == NULL) { progname = argv[0]; } else { ++progname; }
    if (argc > 1) {
	for (ix = 1; ix < argc; ++ix) {
	    file = argv[ix];
	    if ((fp = fopen (file, "r")) == NULL) {
		fprintf (stderr, "%s: %s - %s; skipping ...\n",
				 progname, file, strerror (errno));
		continue;
	    }
	    print_screenfile (fp, file);
	    fclose (fp);
	}
    } else {
	print_screenfile (stdin, "<stdin>");
    }
    return 0;
}
