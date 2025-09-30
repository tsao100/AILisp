QT += widgets
CONFIG += c++17

SOURCES += main.cpp \
           MainWindow.cpp

HEADERS += MainWindow.h

# -------------------------------
# Platform specific settings
# -------------------------------

# --- Linux / Ubuntu ---
unix {
    # If ECL headers are in a nonstandard dir:
    INCLUDEPATH += /usr/include/ecl

    # Linker libs
    LIBS += -lecl -lgmp -lmpfr

    # If ECL is in a custom lib dir:
    # LIBS += -L/path/to/ecl/lib -lecl
}

# --- Windows (MSVC / MinGW) ---
win32 {
    # Adjust this path depending on your ECL install
    INCLUDEPATH += D:/Git/ecl/v24.5.10/vc143-x64

    # Library search path
    LIBS += -LD:/Git/ecl/v24.5.10/vc143-x64

    # Windows libraries usually have `.lib` extensions for MSVC
    # For MinGW, you can often still use -lecl
    msvc: {
        LIBS += ecl.lib
    }
    mingw: {
        LIBS += -lecl -lgmp -lmpfr
    }
}
