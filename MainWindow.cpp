#include "MainWindow.h"
#include <QVBoxLayout>
#include <QMessageBox>

MainWindow::MainWindow(QWidget *parent)
    : QMainWindow(parent)
{
    setWindowTitle("Qt + ECL Command Window (REPL)");

    // Central widget placeholder
    QWidget *central = new QWidget(this);
    setCentralWidget(central);

    // Command Dock
    commandDock = new QDockWidget(tr("Command"), this);
    QWidget *dockContents = new QWidget(commandDock);
    QVBoxLayout *layout = new QVBoxLayout(dockContents);

    historyEdit = new QTextEdit(dockContents);
    historyEdit->setReadOnly(true);
    historyEdit->setMinimumHeight(200);

    inputEdit = new QLineEdit(dockContents);
    inputEdit->setPlaceholderText("Type a Lisp form and press Enter, e.g. (+ 2 3)");

    layout->addWidget(historyEdit);
    layout->addWidget(inputEdit);
    dockContents->setLayout(layout);
    commandDock->setWidget(dockContents);
    addDockWidget(Qt::BottomDockWidgetArea, commandDock);

    // connect enter key
    connect(inputEdit, &QLineEdit::returnPressed, this, &MainWindow::onCommandEntered);

    // Welcome text
    historyEdit->append("ECL Lisp REPL ready. Try: (+ 1 2)   (format t \"Hello~%\")");
}

MainWindow::~MainWindow()
{
}

void MainWindow::onCommandEntered()
{
    QString code = inputEdit->text().trimmed();
    if (code.isEmpty()) return;

    // show input in history
    historyEdit->append(QString(">>> %1").arg(code));
    inputEdit->clear();

    QString result = evaluateLisp(code);
    historyEdit->append(result);
}

// Evaluate Lisp via ECL and return printable QString result
QString MainWindow::evaluateLisp(const QString &lispCode)
{
    // Convert to C string
    QByteArray ba = lispCode.toUtf8();
    const char *cstr = ba.constData();

    // Wrap in progn so user can type forms like:
// (progn (format t "hi") nil) to avoid printing raw ECL objects — but we will attempt to convert result to string.
    // We'll form a call to (with-output-to-string (s) (princ <form> s))
    QString wrapper = QString("(handler-case "
                              "  (with-output-to-string (s) (princ %1 s)) "
                              "  (error (e) (format nil \"ERROR: ~A\" e)))")
                      .arg(QString::fromUtf8(cstr));

    QByteArray wba = wrapper.toUtf8();
    const char *wstr = wba.constData();

    // c_string_to_object builds an ECL object from C string
    cl_object form = c_string_to_object(const_cast<char*>(wstr));
    cl_object res = C_nil;

    // Evaluate and catch errors via ECL-level handler
    // cl_eval will return a cl_object; convert to C string with si_coerce_to_simple_base_string or
    // use princ-to-string result already from wrapper -> returns a simple-string object.

    res = cl_eval(form);

    // Convert result to C string. Use ecl_object_to_cstring if available.
    // ecl functions: (if res is a string), use STR= res->base_string.self?
    // Simpler approach: use cl_object_to_c_string (not standard). We'll try `coerce` to string and extract ptr.
    // Use ecl_native_string_ptr if available: but for portability we use `oocs` helper macro via `c_string_to_object`.
    // We'll convert via `ecl_string_to_locale_string` — but that can be system-dependent.
    // For simplicity, we call `si_string` helpers if available; otherwise, fall back to printing generic representation.

    QString out;

    // Try to call ECL's function to get C string:
    // Note: ecl has function `str_ptr = (char*)ecl_object_to_cstring(res)` on many installs;
    // but since ECL internals differ, we'll try `si_string` family first via forming `(princ-to-string <res>)` approach:
    // Because we already asked Lisp to produce a string (with with-output-to-string and princ), res should be a string.
    if (res != C_nil && !NULLP(res) && ECL_STRINGP(res)) {
        // Use ecl_make_simple_base_string or direct pointer
        cl_object simple = res;
        // ecl_string_ptr is CRUDE; use ECL's function:
        const char *c = (const char*) ecl_base_string_pointer_safe(res);
        if (c) {
            out = QString::fromUtf8(c);
        } else {
            out = QString("<result: (string but pointer null)>");
        }
    } else {
        // Fallback: print the result representation by calling princ-to-string on it
        // Build an expr: (handler-case (princ-to-string <original-form>) (error (e) (format nil "ERROR: ~A" e)))
        QString fallback = QString("(handler-case (princ-to-string %1) (error (e) (format nil \"ERROR: ~A\" e)))")
                           .arg(QString::fromUtf8(cstr));
        cl_object form2 = c_string_to_object(const_cast<char*>(fallback.toUtf8().constData()));
        cl_object res2 = cl_eval(form2);
        if (res2 != C_nil && !NULLP(res2) && ECL_STRINGP(res2)) {
            const char *c2 = (const char*) ecl_base_string_pointer_safe(res2);
            out = QString::fromUtf8(c2 ? c2 : "<null>");
        } else {
            out = QString("<no result>");
        }
    }

    // Make output neat: newline if empty
    if (out.isEmpty()) out = "(nil)";

    return out;
}
