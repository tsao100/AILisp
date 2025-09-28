#-------------------------------------------------
# Qt + ECL Command Window (AutoCAD-like REPL)
#-------------------------------------------------

QT += widgets
CONFIG += c++17

# Source files
SOURCES += main.cpp \
           MainWindow.cpp

# Header files
HEADERS += MainWindow.h \
           ECLWrapper.h

# Include paths for ECL
INCLUDEPATH += /usr/include/ecl

# Link libraries: ECL + GMP/MPFR (adjust if installed elsewhere)
LIBS += -lecl -lgmp -lmpfr

# Fix GMP C++ operator conflict
DEFINES += __GMP_NO_CXX
