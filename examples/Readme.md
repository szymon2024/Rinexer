v1.0.3

EN

This folder contains sample programs for the `obs_rinex3.f90` module
that read the RINEX observation file.

Note: The RINEX file must be provided and its name must be adapted to
the one used by the sample program.

Sample programs:

`obs_rinex3_prog1.f90` reads and prints the RINEX file header.

`obs_rinex3_prog2.f90` prints the observation types from the file
                       header and L1C observations of GPS satellites
                       from the first epoch.

`obs_rinex3_prog3.f90` calculates the ionosphere-free phase combination
                       for GPS satellites for all epochs and
                       saves the result to a text file.


Compilation and run proposal of single sample program:

1. Copy the `obs_rinex3.f90` file from the `src` directory to the
   directory with the sample programs.

2. Execute the following command:

   on Linux
   ```bash
   gfortran obs_rinex3.f90 obs_rinex3_prog1.f90 -o prog1
   ```

   on Windows
   ```cmd
   gfortran obs_rinex3.f90 obs_rinex3_prog1.f90 -o prog1.exe
   ```
   
3. Running the program

   on Linux
   ```bash
   ./prog1
   ```

   on Windows
   ```cmd
   prog1.exe
   ```


PL

Ten folder zawiera przykładowe programy dla modułu obs_rinex3.f90,
które odczytują plik obserwacyjny RINEX.

Uwaga: plik RINEX trzeba dostarczyć i dostosować jego nazwę do
używanej przez przykładowy program.

Przykładowe programy:

`obs_rinex3_prog1.f90` odczytuje i drukuje nagłówek pliku RINEX.

`obs_rinex3_prog2.f90` drukuje typy obserwacji z nagłówka pliku oraz
                       obserwacje L1C satelitów GPS z pierwszej epoki.

`obs_rinex3_prog3.f90` oblicza kombinację fazową wolną od efektu
                       jonosfery dla satelitów GPS dla wszystkich epok i
                       zapisuje wynik do pliku tekstowego.


Propozycja kompilacji i uruchomienia jednego przykładu

1. Skopiuj z katalogu `src` plik `obs_rinex3.f90` do katalogu z
   przykładowymi programami.

2. Wykonaj polecenie:

   w systemie Linux
   ```bash
   gfortran  obs_rinex3.f90 obs_rinex3_prog1.f90 -o prog1
   ```

   w systemie Windows
   ```cmd
   gfortran  obs_rinex3.f90 obs_rinex3_prog1.f90 -o prog1.exe
   ```
   
3. Uruchomienie programu

   w systemie Linux
   ```bash
   ./prog1
   ```

   w systemie Windows
   ```cmd
   prog1.exe
   ```