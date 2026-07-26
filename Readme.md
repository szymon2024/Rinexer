EN:

# Rinexer v1.0.0

Rinexer v1.0.0 is a collection of Fortran modules designed to read
RINEX 3.x observation files. The source code for these modules is
contained within the `*_mod.f90` files.

> ⚠️ **Note:** The current version does not support special events or
comments within the observation data.

## Details
RINEX files are read into structures defined in
`obs_rinex3_types_mod.f90`.

Epoch records are processed sequentially (record by record) and are
not stored permanently in memory.

Demonstrations of how to use these modules are provided in the
following example programs:
- `obs_rinex3_prog1.f90`
- `obs_rinex3_prog2.f90`
- `obs_rinex3_prog3.f90`


Building and Running Examples
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

For more detailed information, please refer to the `Rinexer.txt` file.


PL:

# Rinexer v1.0.0

Rinexer v1.0.0 to kilka modułów Fortran zaprojektowanych do odczytu
plików obserwacyjnych RINEX 3.x. Kod źródłowy dla tych modułów jest
zawarty w plikach `*_mod.f90`.

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


Budowanie i uruchomienie programu przykładowego:
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


