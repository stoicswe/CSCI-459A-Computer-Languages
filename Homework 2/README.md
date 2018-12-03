Project 2 Porgramming Languages

WARNING, some of this file has Russian text in it, my laptop is in Russian.

This project involved writing a scanning program for scanning source code.
This was specifically designed to scan C#.

The C# code was compiled using the following:

taranoshi@rlpt:~/Documents/Houghton College/Programming Languages/Homework 2/Scanner Project$ dotnet build
Microsoft (R) Build Engine версии 15.8.169+g1ccb72aefa для .NET Core
(C) Корпорация Майкрософт (Microsoft Corporation). Все права защищены.

  Восстановление завершено в 53,37 ms для /home/taranoshi/Documents/Houghton College/Programming Languages/Homework 2/Scanner Project/Scanner Project.csproj.
  Scanner Project -> /home/taranoshi/Documents/Houghton College/Programming Languages/Homework 2/Scanner Project/bin/Debug/netcoreapp2.1/Scanner Project.dll

Сборка успешно завершена.
    Предупреждений: 0
    Ошибок: 0

Прошло времени 00:00:01.86
taranoshi@rlpt:~/Documents/Houghton College/Programming Languages/Homework 2/Scanner Project$

Example output is in a text file in the main directory called "Program.csread_out.txt"
the command to run the code was: 

taranoshi@rlpt:~/Documents/Houghton College/Programming Languages/Homework 2/Scanner Project$ dotnet run Program.cs
Done. 10831 tokens extracted.

The challenge with this project I had was working on scanning multiline comments and
strings. I was able to achieve the scanning of this by just ignoring any other char
until I had reached a second '"' or the end of a multiline comment "*/". When it came
to scanning single line comments, I just read the chars until a '\n' char was reached,
then the comment token was returned to the engine.