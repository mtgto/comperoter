四則演算はスタックから2つpopして結果をひとつpushする
割り切れない割り算をするとfloat
浮動小数点を文字として出力するときには切り捨て(65536で割った余り)
0/0=NaN
強制jumpはスタック操作しない
ラベルはそれまでに登場したものにしかジャンプできない
関数は先に定義する
if, whileはどうする？
しんどいので先のラベルへも飛べるように処理系を改造

printInt, printCharはスタック操作をしない

ヒープはスタックとは別に存在している
ヒープは変数置き場として使用する
ヒープのi番目＝i番目の変数
代入先になるときと値を取ってきてスタックトップに置く時がある
関数の引数は相変わらずスタックで、
ヒープの0番目にヒープの先頭アドレスを入れておく
初期化時にheap[0] = 1を入れておく(1番目から使用可能)
変数のread/write時にはheap[heap[0]+変数index]を使用する
関数呼び出し時には呼び出し元でスタックに積んでコール
呼び出し先では引数の数だけheap[heap[0]++] = args[i]を行い、
帰るときにheap[0]を引数の数だけ引く
スタックのトップには関数の戻り値が乗っかる　return文が最後にないといけない(途中にあってもいい)
コンパイル時に見つけられるといいなあ とりあえず最後にあるかチェックだけする?

関数から帰るときは帰り先がわからないので、
関数名ごとに呼び出し元リストを用意しておき、それを利用する
関数f()を呼んで帰る先がlabel1, label2ならば
関数f()の末尾で
def f() {
    ...
    if (*stack - label1 == 0) goto label1
    if (*stack - label2 == 0) goto label2
}
関数の変換はプログラムの最後で行うことにする

比較
if (a == b) => ifzero a - b
if (a > b) => ifneg b - a
if (a < b) => ifneg a - b
小なりイコールとかあるので、if-elseにして入れ替えとか

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

ラベル名は遅延評価で出したいなあ

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

if (exp) { ... }

push exp
ifzero goto label1
...
label1:

while (exp) { ... }

label1:
push exp
ifzero goto label2
...
goto label1
label2:

def func(args) { ... }

func:
...
func_return:
// ヒープの後片付け
dup
jz (call1 - stack[0]) call1
dup
jz (call2 - stack[0]) call1

var a = f(x, y)

if (args.length > 0) {
   push 0
   load
   foreach (arg in args) {
       dup
       push 1
       add
       push arg
       swap
       store
   }
}
push label1
jump label2
label1:
if (args.length > 0) {
   push 0
   load
   push args.length
   sub
   push 0
   store
}
......
label2: // func f
func_return:
