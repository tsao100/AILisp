#include <QApplication>
#include "MainWindow.h"

int main(int argc, char *argv[])
{
    QApplication a(argc, argv);

    // Initialize ECL before creating main window (so Lisp is ready).
    // cl_boot takes argc/argv usually; convert to char** if needed.
    {
        // ECL expects (int argc, char **argv)
        // Passing the application's argv (ok).
        cl_boot(argc, const_cast<char**>(argv));
    }

    MainWindow w;
    w.show();

    int ret = a.exec();

    // Shutdown ECL on exit
    cl_shutdown();

    return ret;
}
