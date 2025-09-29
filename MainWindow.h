#ifndef MAINWINDOW_H
#define MAINWINDOW_H

#include <ecl/ecl.h>

#include <QMainWindow>
#include <QLineEdit>
#include <QPlainTextEdit>
#include <QPushButton>
#include <QTimer>
#include <QPropertyAnimation>
#include <QGraphicsOpacityEffect>
#include <QLabel>

class MainWindow : public QMainWindow {
    Q_OBJECT
public:
    explicit MainWindow(QWidget *parent = nullptr);
    ~MainWindow();

protected:
    void keyPressEvent(QKeyEvent *event) override;
    bool eventFilter(QObject *obj, QEvent *event) override;

private slots:
    void executeCommand();
    void fadeOutResult();

private:
    QPlainTextEdit *consoleOutput;
    QLineEdit *commandInput;
    QPushButton *toggleButton;
    QWidget *consoleWidget;
    QLabel *resultLabel;
    QTimer *fadeTimer;
    QGraphicsOpacityEffect *opacityEffect;
    QGraphicsOpacityEffect *resultOpacityEffect;
    QPropertyAnimation *fadeAnimation;
    QPropertyAnimation *resultFadeAnimation;

    QStringList commandHistory;
    int historyIndex;
    bool consoleVisible;

    void initECL();
    void toggleConsole();
    void showResultTemporarily(const QString &result);
    void defineCADCommands();
};

#endif // MAINWINDOW_H
