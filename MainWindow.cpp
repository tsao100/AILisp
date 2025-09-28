#include "MainWindow.h"
#include <QVBoxLayout>

MainWindow::MainWindow(QWidget *parent)
    : QMainWindow(parent),
    consoleOutput(new QPlainTextEdit(this)),
    commandInput(new QLineEdit(this)) {

    QWidget *central = new QWidget(this);
    QVBoxLayout *layout = new QVBoxLayout(central);

    consoleOutput->setReadOnly(true);
    consoleOutput->setStyleSheet("background:black; color:lightgreen; font-family:monospace;");

    commandInput->setPlaceholderText("Enter Lisp command...");

    layout->addWidget(consoleOutput);
    layout->addWidget(commandInput);
    setCentralWidget(central);

    connect(commandInput, &QLineEdit::returnPressed, this, &MainWindow::executeCommand);

    initECL();
}

MainWindow::~MainWindow() {
    cl_shutdown();
}

void MainWindow::initECL() {
    char *argv[1] = {(char*)"app"};
    cl_boot(1, argv);
    atexit(cl_shutdown);
    consoleOutput->appendPlainText("ECL initialized.\n");
}

void MainWindow::executeCommand() {
    QString cmd = commandInput->text().trimmed();
    if (cmd.isEmpty()) return;

    consoleOutput->appendPlainText(QString("> %1").arg(cmd));
    commandInput->clear();

    // Wrap the Lisp expression to return a string
    QString wrapped = QString("(handler-case (with-output-to-string (s) (princ %1 s)) (error (e) (format nil \"ERROR: ~A\" e)))")
                          .arg(cmd);

    cl_object form = c_string_to_object(wrapped.toUtf8().constData());
    cl_object res = cl_eval(form);

    QString out;
    if (res != Cnil && ECL_STRINGP(res)) {
        const char* cstr = (const char*)ecl_base_string_pointer_safe(res);
        out = QString::fromUtf8(cstr ? cstr : "<null>");
    } else {
        out = "<no result>";
    }

    consoleOutput->appendPlainText(out + "\n");
}
