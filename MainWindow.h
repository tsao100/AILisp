#ifndef MAINWINDOW_H
#define MAINWINDOW_H

#include <QMainWindow>
#include <QLineEdit>
#include <QPlainTextEdit>
#include "ECLWrapper.h"

class MainWindow : public QMainWindow {
    Q_OBJECT
public:
    explicit MainWindow(QWidget *parent = nullptr);
    ~MainWindow();

private slots:
    void executeCommand();

private:
    QPlainTextEdit *consoleOutput;
    QLineEdit *commandInput;

    void initECL();
};

#endif // MAINWINDOW_H
