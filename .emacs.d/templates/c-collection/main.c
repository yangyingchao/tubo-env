#include "utils.h"

int main(int argc, char **argv)
{
    if (argc != 2) {
        printf ("Useage: %s PATH\n", argv[0]);
        return -1;
    }
    printf ("%s\n", get_real_dirname(argv[1]));
    return 0;
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
