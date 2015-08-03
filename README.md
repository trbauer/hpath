# hpath
A path-searching tool (like which).

Hpath is an upgunned version of the venerable `which`.

# Output summary
Without arguments `hpath` reads the `%PATH%` variable and reports useful information
about that namespace including which executables are shadowed and invalid paths.

    % hpath
    C:\Windows\system32
    C:\Windows
      explorer.exe     shadowed by C:\Windows\system32
      hh.exe           shadowed by C:\Windows\system32
      notepad.exe      shadowed by C:\Windows\system32
      regedit.exe      shadowed by C:\Windows\system32
      write.exe        shadowed by C:\Windows\system32
    C:\Windows\System32\Wbem
    C:\Windows\System32\WindowsPowerShell\v1.0\
    C:\Program Files\TortoiseHg\
    ...
    C:\cygwin64\bin
      bunzip2.exe      shadowed by c:\files\rbin
      expand.exe       shadowed by C:\Windows\system32
      find.exe         shadowed by C:\Windows\system32
    C:\Program Files (x86)\Haskell Platform\2014.2.0.0\lib\extralibs\bin
      cabal.exe        shadowed by C:\Users\trbauer\AppData\Roaming\cabal\bin
    C:\Program Files (x86)\Haskell Platform\2014.2.0.0\bin
      haddock.exe      shadowed by C:\Users\trbauer\AppData\Roaming\cabal\bin
    ============= SUMMARY =============
          0 bad path(s)
         45 shadowed exe(s)
       1207 total exe(s)

# Searching for a specific executable
Similar to the `which` command on Unix, if provided an argument, `hpath` will search for that argument, 
intelligently appending `.bat`, `.cmd`, or `.exe` if needed.

    % hpath strings
    C:\Windows\system32
    C:\Windows
    C:\Windows\System32\Wbem
    ...
    C:\cygwin64\bin
      strings.exe
    ...
    C:\Program Files (x86)\Haskell Platform\2014.2.0.0\mingw\bin
      strings.exe
    C:\Program Files (x86)\Haskell Platform\2014.2.0.0\lib\extralibs\bin
    C:\Program Files (x86)\Haskell Platform\2014.2.0.0\bin

The program exits 0 or 1 depending on success in finding a match.

