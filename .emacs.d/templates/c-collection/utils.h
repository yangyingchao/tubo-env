#ifndef _UTILS_H_
#define _UTILS_H_

#include <errno.h>
#include <libgen.h>
#include <limits.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <sys/stat.h>
#include <time.h>

char *get_rand_char(int count);

char *get_real_dirname (const char *in_path);

char** strsplit (const char *str);


#endif /* _UTILS_H_ */
/*
 * Editor modelines
 *
 * Local Variables:
 * c-basic-offset: 4
 * tab-width: 4
 * indent-tabs-mode: nil
 * End:
 *
 * ex: set shiftwidth=4 tabstop=4 expandtab
 * :indentSize=4:tabSize=4:noTabs=true:
 */
