#pragma once

#include <QMainWindow>
#include <QTextEdit>
#include <QLineEdit>
#include <QDockWidget>

extern "C" {
#include <ecl/ecl.h>
}

class MainWindow : public QMainWindow
{
    Q_OBJECT
public:
    MainWindow(QWidget *parent = nullptr);
    ~MainWindow();

private slots:
    void onCommandEntered();

private:
    QDockWidget *commandDock;
    QTextEdit *historyEdit;
    QLineEdit *inputEdit;

    // Evaluate a Lisp string and return textual result (or error string)
    QString evaluateLisp(const QString &lispCode);
};
