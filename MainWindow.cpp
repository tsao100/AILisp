#include "MainWindow.h"
#include <QVBoxLayout>
#include <QHBoxLayout>
#include <QKeyEvent>
#include <QPushButton>

QString eclObjectToQString(cl_object obj) {
    if (obj == Cnil) return "NIL";

    cl_object strObj = cl_princ_to_string(obj);

    if (strObj != Cnil && ECL_STRINGP(strObj) && ECL_BASE_STRING_P(strObj)) {
        const char* cstr = (const char*)ecl_base_string_pointer_safe(strObj);
        if (cstr) {
            return QString::fromUtf8(cstr);
        }
    }

    if (strObj != Cnil && ECL_STRINGP(strObj)) {
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
    commandInput(new QLineEdit(this)),
    toggleButton(new QPushButton("F2", this)),
    resultLabel(new QLabel(this)),
    historyIndex(-1),
    consoleVisible(false) {

    QWidget *central = new QWidget(this);
    QVBoxLayout *layout = new QVBoxLayout(central);
    layout->setContentsMargins(0, 0, 0, 0);
    layout->setSpacing(0);

    // Result label setup (positioned above command input)
    resultLabel->setStyleSheet(
        "QLabel { "
        "background: rgba(0, 0, 0, 180); "
        "color: lightgreen; "
        "font-family: monospace; "
        "font-size: 14px; "
        "padding: 10px; "
        "border: 1px solid green; "
        "border-radius: 5px; "
        "}"
        );
    resultLabel->setWordWrap(true);
    resultLabel->setAlignment(Qt::AlignLeft | Qt::AlignTop);
    resultLabel->setVisible(false);
    resultLabel->setMinimumHeight(100);
    resultLabel->setMaximumHeight(400);
    resultLabel->setSizePolicy(QSizePolicy::Expanding, QSizePolicy::Expanding);

    // Setup result fade animation
    resultOpacityEffect = new QGraphicsOpacityEffect(resultLabel);
    resultLabel->setGraphicsEffect(resultOpacityEffect);
    resultOpacityEffect->setOpacity(1.0);

    resultFadeAnimation = new QPropertyAnimation(resultOpacityEffect, "opacity", this);
    resultFadeAnimation->setDuration(1000);
    resultFadeAnimation->setStartValue(1.0);
    resultFadeAnimation->setEndValue(0.0);

    // Console output setup (hidden by default, shows above command input)
    consoleOutput->setReadOnly(true);
    consoleOutput->setStyleSheet("background:black; color:lightgreen; font-family:monospace;");
    consoleOutput->setVisible(false);
    consoleOutput->setSizePolicy(QSizePolicy::Expanding, QSizePolicy::Expanding);

    // Setup console fade animation
    opacityEffect = new QGraphicsOpacityEffect(consoleOutput);
    consoleOutput->setGraphicsEffect(opacityEffect);
    opacityEffect->setOpacity(1.0);

    fadeAnimation = new QPropertyAnimation(opacityEffect, "opacity", this);
    fadeAnimation->setDuration(1000);
    fadeAnimation->setStartValue(1.0);
    fadeAnimation->setEndValue(0.0);

    // Command input setup (always visible at bottom)
    commandInput->setPlaceholderText("Enter Lisp command or CAD command (e.g., 'line')...");
    commandInput->setStyleSheet(
        "QLineEdit { "
        "background: rgba(0, 0, 0, 200); "
        "color: white; "
        "font-family: monospace; "
        "font-size: 14px; "
        "padding: 8px; "
        "border: 2px solid gray; "
        "border-radius: 3px; "
        "}"
        );
    commandInput->setSizePolicy(QSizePolicy::Expanding, QSizePolicy::Fixed);

    // Toggle button setup
    toggleButton->setStyleSheet(
        "QPushButton { "
        "background: rgba(0, 0, 0, 200); "
        "color: white; "
        "font-family: monospace; "
        "font-size: 14px; "
        "padding: 8px 15px; "
        "border: 2px solid gray; "
        "border-radius: 3px; "
        "min-width: 50px; "
        "} "
        "QPushButton:hover { "
        "background: rgba(50, 50, 50, 200); "
        "border: 2px solid lightgray; "
        "} "
        "QPushButton:pressed { "
        "background: rgba(100, 100, 100, 200); "
        "}"
        );
    toggleButton->setToolTip("Toggle console (F2)");
    toggleButton->setSizePolicy(QSizePolicy::Fixed, QSizePolicy::Fixed);

    // Bottom layout: command input + toggle button
    QHBoxLayout *bottomLayout = new QHBoxLayout();
    bottomLayout->setSpacing(5);
    bottomLayout->setContentsMargins(5, 5, 5, 5);
    bottomLayout->addWidget(commandInput);
    bottomLayout->addWidget(toggleButton);

    QWidget *bottomWidget = new QWidget(this);
    bottomWidget->setLayout(bottomLayout);

    // Layout: stretch space, console output, result label, command input+button (bottom)
    layout->addStretch();
    layout->addWidget(consoleOutput);
    layout->addWidget(resultLabel);
    layout->addWidget(bottomWidget, 0, Qt::AlignBottom);

    setCentralWidget(central);

    // Setup fade timer
    fadeTimer = new QTimer(this);
    fadeTimer->setSingleShot(true);
    fadeTimer->setInterval(3000);

    connect(commandInput, &QLineEdit::returnPressed, this, &MainWindow::executeCommand);
    connect(toggleButton, &QPushButton::clicked, this, &MainWindow::toggleConsole);
    connect(fadeTimer, &QTimer::timeout, this, &MainWindow::fadeOutResult);
    connect(resultFadeAnimation, &QPropertyAnimation::finished, this, [this]() {
        resultLabel->setVisible(false);
        resultOpacityEffect->setOpacity(1.0);
    });

    // Install event filter on commandInput to handle special keys
    commandInput->installEventFilter(this);

    initECL();
}

MainWindow::~MainWindow() {
    cl_shutdown();
}

void MainWindow::initECL() {
    char *argv[1] = {(char*)"app"};
    cl_boot(1, argv);
    atexit(cl_shutdown);

    defineCADCommands();

    // Don't show initialization message in console, just in result label
    showResultTemporarily("ECL initialized. Press F2 to toggle console.");
}

void MainWindow::defineCADCommands() {
    // Define a "line" command in Lisp
    const char* lineCommand = R"(
        (defun line (&optional p1 p2)
          (cond
            ((and p1 p2)
             (format nil "Drawing line from ~A to ~A" p1 p2))
            (p1
             (format nil "Line started at ~A. Specify next point." p1))
            (t
             "LINE command: Specify first point")))
    )";

    cl_object form = c_string_to_object(lineCommand);
    cl_eval(form);

    // Define more CAD commands as needed
    const char* circleCommand = R"(
        (defun circle (&optional center radius)
          (cond
            ((and center radius)
             (format nil "Drawing circle at ~A with radius ~A" center radius))
            (center
             (format nil "Circle center at ~A. Specify radius." center))
            (t
             "CIRCLE command: Specify center point")))
    )";

    form = c_string_to_object(circleCommand);
    cl_eval(form);
}

void MainWindow::keyPressEvent(QKeyEvent *event) {
    if (event->key() == Qt::Key_F2) {
        toggleConsole();
        event->accept();
    } else {
        QMainWindow::keyPressEvent(event);
    }
}

bool MainWindow::eventFilter(QObject *obj, QEvent *event) {
    if (obj == commandInput && event->type() == QEvent::KeyPress) {
        QKeyEvent *keyEvent = static_cast<QKeyEvent*>(event);

        // Handle Up Arrow - navigate backwards in history
        if (keyEvent->key() == Qt::Key_Up) {
            if (!commandHistory.isEmpty() && historyIndex < commandHistory.size() - 1) {
                historyIndex++;
                commandInput->setText(commandHistory[commandHistory.size() - 1 - historyIndex]);
            }
            return true; // Event handled
        }

        // Handle Down Arrow - navigate forwards in history
        if (keyEvent->key() == Qt::Key_Down) {
            if (historyIndex > 0) {
                historyIndex--;
                commandInput->setText(commandHistory[commandHistory.size() - 1 - historyIndex]);
            } else if (historyIndex == 0) {
                historyIndex = -1;
                commandInput->clear();
            }
            return true; // Event handled
        }

        // Handle Spacebar when input is empty - repeat last command
        if (keyEvent->key() == Qt::Key_Space && commandInput->text().isEmpty()) {
            if (!commandHistory.isEmpty()) {
                commandInput->setText(commandHistory.last());
                executeCommand();
            }
            return true; // Event handled
        }
    }

    // Pass the event on to the parent class
    return QMainWindow::eventFilter(obj, event);
}

void MainWindow::toggleConsole() {
    consoleVisible = !consoleVisible;
    consoleOutput->setVisible(consoleVisible);

    if (consoleVisible) {
        // Stop any ongoing fade and restore opacity
        fadeTimer->stop();
        fadeAnimation->stop();
        opacityEffect->setOpacity(1.0);
    }

    // Command input always stays focused
    commandInput->setFocus();
}

void MainWindow::executeCommand() {
    QString cmd = commandInput->text().trimmed();
    if (cmd.isEmpty()) return;

    // Add to command history
    if (commandHistory.isEmpty() || commandHistory.last() != cmd) {
        commandHistory.append(cmd);
    }
    historyIndex = -1; // Reset history navigation

    commandInput->clear();

    // Check if it's a CAD-style command (starts with a word, not parenthesis)
    QString wrapped;
    if (!cmd.startsWith('(')) {
        // CAD command style - convert to function call
        QStringList parts = cmd.split(QRegExp("\\s+"));
        QString funcName = parts[0].toLower();
        QStringList args = parts.mid(1);

        // Build function call with arguments
        if (args.isEmpty()) {
            wrapped = QString("(%1)").arg(funcName);
        } else {
            QString argStr = args.join(" ");
            wrapped = QString("(%1 %2)").arg(funcName, argStr);
        }
    } else {
        wrapped = cmd;
    }

    // Wrap in handler-case for error handling
    QString safewrapped = QString(
                              "(handler-case %1 "
                              "(error (e) (format nil \"ERROR: ~A\" e)))").arg(wrapped);

    QString out;
    bool hasError = false;

    try {
        cl_object form = c_string_to_object(safewrapped.toUtf8().constData());

        if (form == Cnil || form == NULL) {
            out = "ERROR: Failed to parse command";
            hasError = true;
        } else {
            cl_object res = Cnil;

            // Use ECL's error handling
            CL_CATCH_ALL_BEGIN(ecl_process_env()) {
                res = cl_eval(form);
            } CL_CATCH_ALL_IF_CAUGHT {
                out = "ERROR: Exception during evaluation";
                hasError = true;
            } CL_CATCH_ALL_END;

            if (!hasError) {
                if (res == NULL) {
                    out = "ERROR: Evaluation returned NULL";
                    hasError = true;
                } else {
                    out = eclObjectToQString(res);
                    // Check if the result indicates an error from handler-case
                    if (out.startsWith("ERROR:", Qt::CaseInsensitive)) {
                        hasError = true;
                    }
                }
            }
        }
    } catch (const std::exception &e) {
        out = QString("ERROR: C++ exception - %1").arg(e.what());
        hasError = true;
    } catch (...) {
        out = "ERROR: Unknown exception during evaluation";
        hasError = true;
    }

    // Always log to console output
    consoleOutput->appendPlainText(QString("> %1").arg(cmd));
    consoleOutput->appendPlainText(out + "\n");

    // If console is not visible, show result in label
    if (!consoleVisible) {
        showResultTemporarily(out);
    }
}

void MainWindow::showResultTemporarily(const QString &result) {
    // Show result in floating label
    resultLabel->setText(result);
    resultLabel->setVisible(true);
    resultOpacityEffect->setOpacity(1.0);

    // Adjust label height based on content
    QFontMetrics fm(resultLabel->font());
    int textHeight = fm.boundingRect(
                           resultLabel->contentsRect(),
                           Qt::TextWordWrap | Qt::AlignLeft | Qt::AlignTop,
                           result
                           ).height();

    // Set height with padding, respecting min/max
    int desiredHeight = qBound(100, textHeight + 30, 400);
    resultLabel->setFixedHeight(desiredHeight);

    // Start fade timer
    QTimer::singleShot(3000, this, [this]() {
        resultFadeAnimation->start();
    });
}

void MainWindow::fadeOutResult() {
    // This is for console fade (if needed in future)
    fadeAnimation->start();
}
