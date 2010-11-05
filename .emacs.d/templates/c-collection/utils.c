#include "utils.h"

/**
 * get_rand_char - Get a string composed by random char.
 * @count - Length of string.
 *
 * Return: char*
 * Remeber to free it after use.
 */
char *get_rand_char(int count)
{
    char *buf = NULL;
    char c;
    int i, val;
    if (count < 0)
        count = 1;
    buf = (char *)malloc(count+1);
    if (buf == NULL) {
        fprintf(stderr, "ERROR: Failed to alloc memory for string.\n");
        return NULL;
    }
    memset(buf, 0, count+1);
    srand(time(NULL));
    for (i = 0; i < count; i++) {
        val = rand();
        c  = val%93 + 33;
        *(buf + i) = c;
    }
    return buf;
}

/**
 * get_real_path - Get absolute path of in_path.
 * @in_path - Character in path
 *
 * Return: char*
 */
char *get_real_dirname(const char *in_path)
{
    char rest_name[1024] = {'\0'}, tmp[1024] = {'\0'};
    char *base_name = NULL, *rp = NULL,  *real_path = NULL;
    char *dir_name = dirname(strdup(in_path));
    int real_path_len = 0;

    while ((rp = realpath(dir_name, NULL)) == NULL) {
        memset(tmp, 0, 1024);
        base_name = basename(dir_name);
        if (strlen(rest_name)) {
            sprintf(tmp, "%s/%s", base_name, rest_name);
            strncpy(rest_name, tmp, strlen(tmp)+1);
        }
        else {
            sprintf(rest_name, "%s", base_name);
        }
        dir_name = dirname(dir_name);
    }

    real_path_len = strlen(rp) + strlen(rest_name) + 2;
    real_path = calloc(real_path_len, sizeof(char));
    sprintf(real_path, "%s/%s", rp, rest_name);
    return real_path;
}

/**
 * strsplit - Split a string into an array, splitted by whitspace.
 * @str - Character string
 *
 * Return: char**
 *
 * NOTE: The leading character will be stripped if its is one of the
 * following:
 * ['=', '<', '>']
 */
char **strsplit(const char *str)
{
    int i=0, num=0;
    char **array = NULL;
    char *ptr = NULL, *p = NULL;
    char tmp_str[strlen(str)];

    memset(tmp_str, 0, strlen(str));

    strcpy(tmp_str, str);
    p = tmp_str;
    /* Compute total number of white spaces */
    while ((ptr = strchr(p, ' ')) != NULL) {
        num++;
        p = ptr + 1;
    }

    /* Alloc memory for new array */
    array = (char **)calloc(num+2, sizeof(char *));

    /* Strip some leading characters */
    p = tmp_str;
    if (*p == '=' || *p == '<' || *p == '>') {
        p++;
    }
    /* Copy components of string into array. */
    while ((ptr = strchr(p, ' ')) != NULL) {
        *ptr = '\0';
        array[i] = strdup(p);
        p = ptr + 1;
        i++;
    }
    /* XXX: Append the last component if it is not obscurer character.*/
    if ((strlen(p) != 1) || (*p >= '0')) {
        array[i++]=strdup(p);
    }
    array[i] = 0;
    return array;
}

/**
 * free_array - Free allocated memory (by str_split).
 * @array - Character array
 *
 * Return: void
 */
void free_array(char **array)
{
    if (array != NULL) {
        int i = 0;
        while (array[i]) {
            free(array[i]);
            i++;
        }
        free(array[i]);
    }
}

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
