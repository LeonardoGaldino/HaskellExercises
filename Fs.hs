module Fs where

type Name = String
type Content = String

data File = SimpleFile Name Content | Folder Name [File]

type FileSystem = [File]

f1, f2, f3 :: File 
f1 = SimpleFile "config.sys" "xyzxyz"  
f2 = Folder "Windows" [SimpleFile "win.ini" "xxx",
                          SimpleFile "win.exe" "sss"] 
f3 = Folder "Windows" [SimpleFile "win.ini" "xxx", 
                          Folder "Foo" [SimpleFile "kkk.exe" "kbum"]]
fileSys :: FileSystem
fileSys = [SimpleFile   "config.sys" "xyzxyz", 
           SimpleFile   "autoexec.bat" "bbb", 
           Folder  "Windows" [SimpleFile   "win.ini" "xxx", 
                                SimpleFile   "win.exe" "sss",
                                Folder  "maisUmDir" []
                               ], 
           SimpleFile   "autoexec.bak" "rrr" 
          ]

fileSys2 :: FileSystem
fileSys2 = [ Folder  "Windows" [SimpleFile   "win.ini" "xxx", 
             Folder  "etc" [Folder  "h"[]]  ,
             Folder  "maisUmDir" []] 
           ]