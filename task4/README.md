## Задание "Стековая машина"
Интеграция двух частей: транслятор арифметических выражений (с переменными, имена которых суть строки из букв) в исполняемый код стековой ВМ.

В парсер арифметических выражений была добавлена возможность указания переменных.
За основу стековой машины взята самая первая моя версия.

Транслятор в исполняемый код стековой ВМ находится в ToStack.hs

Пример.
executeCommands (translateSumToCommands $ parseSum "2+4*5+3*x") initialState
