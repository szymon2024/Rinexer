EN:

# Rinexer v1.0.1

Rinexer v1.0.1 is a collection of Fortran modules designed for reading 
RINEX 3.x observation files. The source code for these modules is 
contained in the `rinexer_src` directory.

> ⚠️ **Note:** The current version does not support handling special events and 
comments within the observation data.

The RINEX file is read into the data structures defined in the 
`obs_rinex3_types_mod.f90` file.

Epoch records are read record by record, meaning they are not accumulated 
in memory.

Example usage is demonstrated in the following programs:
- `obs_rinex3_prog1.f90`
- `obs_rinex3_prog2.f90`
- `obs_rinex3_prog3.f90`

located in the `examples` directory.


Building and running the example program in the `examples` directory:

Linux
```bash
make -f Makefile.obs_rinex3_prog1
./obs_rinex3_prog1
```

Windows (MinGW)
```cmd
mingw32-make -f Makefile.obs_rinex3_prog1_win
obs_rinex3_prog1.exe
```

More information can be found in the `Rinexer.txt` file.


PL:

# Rinexer v1.0.1

Rinexer v1.0.1 to kilka modułów Fortran zaprojektowanych do odczytu
plików obserwacyjnych RINEX 3.x. Kod źródłowy dla tych modułów jest
zawarty katalogu `rinexer_src`.

> ⚠️ **Uwaga:** Obecna wersja nie wspiera obsługi zdarzeń specjalnych i
komentarzy w danych obserwacyjnych.

Plik RINEX jest odczytywany do danych określonych w pliku
`obs_rinex3_types_mod.f90`.

Rekordy epok są odczytywane rekord po rekordzie tzn. nie są gromadzone
w pamięci.

Przykładowe użycie zostało zaprezentowane w programach:
- `obs_rinex3_prog1.f90`
- `obs_rinex3_prog2.f90`
- `obs_rinex3_prog3.f90`

znajdujących się w katalogu `examples`.


Budowanie i uruchomienie programu przykładowego w katalogu `examples`:

Linux
```bash
make -f Makefile.obs_rinex3_prog1
./obs_rinex3_prog1
```

Windows (MinGW)
```cmd
mingw32-make -f Makefile.obs_rinex3_prog1_win
obs_rinex3_prog1.exe
```

Więcej informacji w pliku `Rinexer.txt`.


