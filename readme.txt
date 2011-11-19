四則演算はスタックから2つpopして結果をひとつpushする
割り切れない割り算をするとfloat
浮動小数点を文字として出力するときには切り捨て(65536で割った余り)
0/0=NaN
強制jumpはスタック操作しない
ラベルはそれまでに登場したものにしかジャンプできない
関数は先に定義する
if, whileはどうする？
ヒープはスタックとは別に存在している

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

関数を導入
関数呼ぶ前に戻ってくる位置にラベルを設置、その値をスタックに積む
関数は実装のあとに置かれるため、ジャンプする先のラベルの計算はコンパイル後に行う

Program::= List[Statement]
Statement::= FUNC | DEF | SUBSTITUTE | WHILE | IF | PRINT | (RETURN)
FUNC::= def FUNCNAME({VAR}) { Statement* } // 引数 + 実装
DEF: var ID = EXP;
SUBSTITUTDE: ID = EXP; // 代入
WHILE: while (EXP) { Statement* } // whileループ
IF: if (EXP) { Statement* } // if文(式にするべき？)
PRINT_INT: printInt NUM;
PRINT_CHAR: printChar NUM;
RETURN: return EXP;

EXP::= VAR | 
       NUM |
       ( EXP ) |
        EXP + EXP | EXP - EXP | EXP * EXP | EXP / EXP | EXP % EXP |
	VAR(EXP*)

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

if (exp) { … }

push exp
ifzero goto label1
…
label1:


while (exp) { … }

label1:
push exp
ifzero goto label2
…
goto label1
label2:


var a = f(x, y)

push x
push y
push label1
jump label2
label1:
pop // y
pop // x
......
label2: // func f
...
ret
