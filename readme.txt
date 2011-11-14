四則演算はスタックから2つpopして結果をひとつpushする
割り切れない割り算をするとfloat
浮動小数点を文字として出力するときには切り捨て(65536で割った余り)
0/0=NaN
強制jumpはスタック操作しない

コンパイラの実装
文の評価時にはvarの数だけスタックに積まれてるようにする
簡単にするため、変数宣言はプログラムの最初にしかできない（途中に出てきたらエラー）
var x, iが定義されていたら文を評価するときにはスタックの状態はx, iの順に積まれてなければならない
変数に入れられるのは数値のみ
　将来、文字列、配列を変数が指すときには最初にサイズを確保する
　　変数の型もないと駄目？
0のとき実行するif文はサブルーチンへジャンプ＆リターン
0じゃないとき実行するif文は0のときにif文のjump先をif文内部の最後にする
サブルーチン内で積んだスタックはすべて元に戻さないといけない

Program: List[Statement]
Statement: DEF | WHILE | IF | PRINT
DEF: var ID = NUM
WHILE: while (EXP) { Statement* }
IF: if (EXP) { Statement* }
PRINT: print NUM | STRING

EXP = VAR | EXP + EXP | EXP - EXP | EXP * EXP | EXP / EXP | EXP % EXP

mod
jmpifzero fizz
dup
...
jmp loop
end

fizz:
push 'F'
printChar
push 'i'
printChar
...
jmp fizznext


fizzbuzz
var x = 100
var i = 1

while (i < x) {
      if (i % 3 == 0) {
      	 print "Fizz"
      }
      if (i % 5 == 0) {
      	 print "Buzz"
      }
      if ((i % 3) * (i % 5)) {
      	 print i
      }
      print '\n'
      i++
}
