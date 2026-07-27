EN:

# Rinexer v1.0.2

Rinexer v1.0.2 is a Fortran module designed for reading and writing
RINEX 3.x observation files.

> ⚠️ **Note:** The current version does not support handling special
events and comments within the observation data.

The source code for the module is contained in the `rinexer_src`
directory.

Using Rinexer requires writing a program in Fortran, compiling the
`obs_rinex3_mod.f90` module into object file `obs_rinex3_mod.o`, and
compiling the main program with `obs_rinex3_mod.o` into an executable.

## On Linux:

1. Copy the rinexer_src directory into your project directory.

2. Navigate to the rinexer_src directory and compile the module:
   
   ```bash
   gfortran -c obs_rinex3_mod.f90
   ```

3. Return to the main project directory and write a Fortran program
   that uses the aforementioned module.

4. Compile the main program, linking it with the compiled module and
   including the module path (using the -I flag):

   ```bash
   gfortran -Irinexer_src my_program.f90 rinexer_src/obs_rinex3_mod.o -o my_program
   ```

5. Run the program:

   ```bash
   ./my_program
   ```

## On Windows:

1. Copy the `rinexer_src` directory into your project directory.

2. Navigate to the `rinexer_src` directory and compile
   `obs_rinex3_mod.f90`. For the gfortran compiler in Command Prompt
   or PowerShell, use the following commands:

   ```cmd
   gfortran -c obs_rinex3_mod.f90
   ```

3. Return to the main project directory and write a Fortran program
   that uses the aforementioned module.

4. Compile the main program, linking it with the compiled module and
   including the module path

   ```cmd
   gfortran -Irinexer_src my_program.f90 rinexer_src\obs_rinex3_mod.o -o my_program.exe
   ```

5. Run the program:

   ```cmd
   my_program.exe
   ```


More information can be found in the `Rinexer.txt` file.


PL:

# Rinexer v1.0.2

Rinexer v1.0.2 to moduł Fortran zaprojektowany do odczytu plików
obserwacyjnych RINEX 3.x.

> ⚠️ **Uwaga:** Obecna wersja nie wspiera obsługi zdarzeń specjalnych i
komentarzy w danych obserwacyjnych.

Kod źródłowy modułu jest zawarty katalogu `rinexer_src`.

Użycie Rinexera wymaga napisania programu w języku Fortran,
skompilowania modułu `obs_rinex3_mod.f90` do pliku obiektowego
`obs_rinex3_mod.o` i skompilowaniu samego programu z
`obs_rinex3_mod.o` do pliku wykonywalnego np.

## W systemie Linux

1. Skopiuj katalog `rinexer_src` do katalogu swojego projektu.

2. Przejdź do katalogu `rinexer_src` i skompiluj moduł:

   ```bash
   gfortran -c obs_rinex3_mod.f90
   ```
   
3. Wróć do głównego katalogu projektu i napisz program w Fortranie
   korzystający z modułu.
   
4. Skompiluj program główny łącząc go ze plikiem obiektowym:

   ```bash
   gfortran -Irinexer_src moj_program.f90 rinexer_src/obs_rinex3_mod.o -o moj_program
   ```
   
5. Uruchom program

   ```bash
   ./moj_program
   ```

## W systemie Windows

1. Skopiuj katalog `rinexer_src` do katalogu swojego projektu.

2. Przejdź do katalogu `rinexer_src` i skompiluj moduł:

   ```cmd
   gfortran -c obs_rinex3_mod.f90
   ```

3. Wróć do głównego katalogu projektu i napisz program w Fortranie
   korzystający z modułu.

4. Skompiluj program główny łącząc go z plikiem obiektowym

   ```cmd
   gfortran -Irinexer_src moj_program.f90 rinexer_src\obs_rinex3_mod.o -o moj_program.exe
   ```

5. Uruchom program

   ```cmd
   moj_program.exe
   ```


Więcej informacji w pliku `Rinexer.txt`.


