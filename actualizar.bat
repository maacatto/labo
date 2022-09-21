@echo off

git add .
if %errorlevel% NEQ 0 (goto END)  
rem Agrego a staging todo lo que vengo trabajando y no subi a github aun. OJO, despues del add hay un espacio y un punto

git commit -m "algun mensaje de lo que voy a subir"

git fetch upstream main 
rem Descarga cambios nuevos en el repo de los profes

git merge upstream/main 
rem En este paso puede llegar a abrir un editor con un mensaje que dice que mezcla commits
rem Alcanza con guardar el archivo y salir del editor

git push 
rem Sube cambios propios con los nuevos del otro repo


git pull 
rem traer el repositorio a la maquina

:END

pause