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

Using Rinexer requires writing a program in Fortran, compiling the
individual modules into *.o files, and compiling the main program into
an executable file e.g.,

on Linux:

1. Copy the rinexer_src directory into your project directory.

2. Navigate to the rinexer_src directory and compile the individual
   modules. For the gfortran compiler, use the following commands:
   
   ```bash
   gfortran -c obs_rinex3_types_mod.f90
   gfortran -c obs_rinex3_indexes_mod.f90
   gfortran -c obs_rinex3_reader_mod.f90
   gfortran -c obs_rinex3_writer_mod.f90
   ```

3. Return to the main project directory and write a Fortran program
   that uses the aforementioned modules.

4. Compile the main program, linking it with the compiled modules and
   including the module path (using the -I flag):

   ```bash
   gfortran -Irinexer_src rinexer_src/obs_rinex3_types_mod.o rinexer_src/obs_rinex3_indexes_mod.o rinexer_src/obs_rinex3_reader_mod.o rinexer_src/obs_rinex3_writer_mod.o my_program.f90 -o my_program
   ```

5. Run the program:

   ```bash
   ./moj_program
   ```

on Windows:

1. Copy the rinexer_src directory into your project directory.

2. Navigate to the rinexer_src directory and compile the individual
   modules. For the gfortran compiler in Command Prompt or PowerShell,
   use the following commands:

   ```cmd
   gfortran -c obs_rinex3_types_mod.f90
   gfortran -c obs_rinex3_indexes_mod.f90
   gfortran -c obs_rinex3_reader_mod.f90
   gfortran -c obs_rinex3_writer_mod.f90
   ```

3. Return to the main project directory and write a Fortran program
   that uses the aforementioned modules.

4. Compile the main program, linking it with the compiled modules and
   including the module path

   ```cmd
   gfortran -Irinexer_src rinexer_src\obs_rinex3_types_mod.o rinexer_src\obs_rinex3_indexes_mod.o rinexer_src\obs_rinex3_reader_mod.o rinexer_src\obs_rinex3_writer_mod.o my_program.f90 -o my_program.exe
   ```

5. Run the program:

   ```cmd
   my_program.exe
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

Użycie Rinexera wymaga napisania programu w języku Fortran,
skompilowania poszczególnych modułów do plików *.o i skompilowaniu
samego programu do pliku wykonywalnego np.

W systemie Linux

1. Skopiuj katalog `rinexer_src` do katalogu swojego projektu.

2. Przejdź do katalogu `rinexer_src` i skompiluj poszczególne moduły
   Dla kompilatora gfortran będą to polecenia:

   ```bash
   gfortran -c obs_rinex3_types_mod.f90
   gfortran -c obs_rinex3_indexes_mod.f90
   gfortran -c obs_rinex3_reader_mod.f90
   gfortran -c obs_rinex3_writer_mod.f90
   ```
   
3. Wróć do głównego katalogu projektu i napisz program w Fortranie
   korzystający z wyżej wymienionych modułów.
   
4. Skompiluj program główny, łącząc go ze skompilowanymi modułami:

   ```bash
   gfortran -Irinexer_src rinexer_src/obs_rinex3_types_mod.o rinexer_src/obs_rinex3_indexes_mod.o rinexer_src/obs_rinex3_reader_mod.o rinexer_src/obs_rinex3_writer_mod.o moj_program.f90 -o moj_program
   ```
   
5. Uruchom program

   ```bash
   ./moj_program
   ```

W systemie Windows

1. Skopiuj katalog `rinexer_src` do katalogu swojego projektu.

2. Przejdź do katalogu `rinexer_src` i skompiluj poszczególne
   moduły. Dla kompilatora gfortran w wierszu poleceń (Command Prompt /
   PowerShell) będą to polecenia:

   ```cmd
   gfortran -c obs_rinex3_types_mod.f90
   gfortran -c obs_rinex3_indexes_mod.f90
   gfortran -c obs_rinex3_reader_mod.f90
   gfortran -c obs_rinex3_writer_mod.f90
   ```cmd

3. Wróć do głównego katalogu projektu i napisz program w Fortranie
   korzystający z wyżej wymienionych modułów.

4. Skompiluj program główny, wskazując lokalizację plików modułów oraz
   łącząc go z plikami obiektowymi

   ```cmd
   gfortran -Irinexer_src rinexer_src\obs_rinex3_types_mod.o rinexer_src\obs_rinex3_indexes_mod.o rinexer_src\obs_rinex3_reader_mod.o rinexer_src\obs_rinex3_writer_mod.o moj_program.f90 -o moj_program.exe
   ```

5. Uruchom program

   ```cmd
   moj_program.exe
   ```


Więcej informacji w pliku `Rinexer.txt`.


