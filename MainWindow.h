#ifndef MAINWINDOW_H
#define MAINWINDOW_H

#include <ecl/ecl.h>

#include <QMainWindow>
#include <QLineEdit>
#include <QPlainTextEdit>

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
