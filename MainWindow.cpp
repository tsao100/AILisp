#include "MainWindow.h"
#include <QVBoxLayout>

QString eclObjectToQString(cl_object obj) {
    if (obj == Cnil) return "NIL";

    // Convert to string using princ-to-string
    cl_object strObj = cl_princ_to_string(obj);

    if (strObj != Cnil && ECL_STRINGP(strObj) && ECL_BASE_STRING_P(strObj)) {
        const char* cstr = (const char*)ecl_base_string_pointer_safe(strObj);
        if (cstr) {
            return QString::fromUtf8(cstr);
        }
    }

    // Fallback: if it's not a base string or conversion failed,
    // try to coerce to base string
    if (strObj != Cnil && ECL_STRINGP(strObj)) {
        // Use ECL's string conversion
        cl_object base_str = si_coerce_to_base_string(strObj);
        if (base_str != Cnil && ECL_BASE_STRING_P(base_str)) {
            const char* cstr = (const char*)ecl_base_string_pointer_safe(base_str);
            if (cstr) {
                return QString::fromUtf8(cstr);
            }
        }
    }

    return "<unconvertible>";
}

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

    consoleOutput->appendPlainText(QString("指令: %1").arg(cmd));
    commandInput->clear();

    // Wrap command in handler-case to catch Lisp errors
    QString wrapped = QString(
                          "(handler-case (with-output-to-string (s) (princ %1 s)) "
                          "(error (e) (format nil \"ERROR: ~A\" e)))").arg(cmd);

    cl_object form = c_string_to_object(wrapped.toUtf8().constData());

    cl_object res = Cnil;
    try {
        res = cl_eval(form);
        QString out = eclObjectToQString(res);
        consoleOutput->appendPlainText(out + "\n");
    } catch (...) {
        consoleOutput->appendPlainText("Error evaluating expression.\n");
    }
}
