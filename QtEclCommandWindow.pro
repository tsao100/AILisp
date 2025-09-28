QT += widgets
CONFIG += c++17

SOURCES += main.cpp \
           MainWindow.cpp

HEADERS += MainWindow.h

# If ECL headers are in a nonstandard dir:
INCLUDEPATH += /usr/include/ecl

# Linker libs - adjust if necessary
LIBS += -lecl
# and possibly
LIBS += -lgmp -lmpfr

# If ECL is installed in a custom lib dir:
# LIBS += -L/path/to/ecl/lib -lecl
