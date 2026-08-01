
#
EN
v.1.0.3

obs_rinex3.f90 is a Fortran module for reading and writing RINEX 3.x
               observation files which can be used in a Fortran
               program (code).

> ⚠️ **Note:** The current version does not support handling special
events and comments within the observation data.

Application examples are in the `examples` folder.


HOW TO USE THE obs_rinex3 MODULE

1. Open the `obs_rinex3.f90` file and see what data types and
   procedures the module offers. They are declared as public.

2. Copy the `obs_rinex3.f90` file to your project directory.

3. Add a usage declaration in your code:

   ```fortran
   program my_program
       use obs_rinex3
   ```

4. Compile your program by providing both source files at the same
   time:
   
   On Linux
   ```bash
   gfortran obs_rinex3.f90 my_program.f90 -o rinex_app
   ```

   On Windows
   ```bash
   gfortran obs_rinex3.f90 my_program.f90 -o rinex_app.exe
   ```

#
PL
v.1.0.3

obs_rinex3.f90 to moduł języka Fortran do odczytu i zapisu plików
               obserwacyjnych RINEX 3.x, który można używać
               w programie (kodzie) języka Fortran. Przykłady
               zastosowania są w folderze `examples`.

> ⚠️ **Uwaga:** Obecna wersja nie obsługuje obsługi zdarzeń specjalnych
  i komentarzy w danych obserwacyjnych.


JAK UŻYWAĆ MODUŁU obs_rinex3

1. Otwórz plik `obs_rinex3.f90` i sprawdź, jakie typy danych
   i procedury oferuje moduł. Są one zadeklarowane jako publiczne.

2. Skopiuj plik `obs_rinex3.f90` do katalogu swojego projektu.

3. Dodaj deklarację użycia w swoim kodzie:

   ```fortran
   program moj_program
       use obs_rinex3
   ```

4. Skompiluj swój program, podając oba pliki jednocześnie:
   
   w systemie Linux
   ```bash
   gfortran obs_rinex3.f90 moj_program.f90 -o rinex_app
   ```

   w systemie Windows
   ```bash
   gfortran obs_rinex3.f90 moj_program.f90 -o rinex_app.exe
   ```