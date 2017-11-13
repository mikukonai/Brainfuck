#lang racket

; A simple Brainfuck interpreter
; 简单的Brainfuck解释器
;
; 2017.11.14 Mikukonai
; https://github.com/mikukonai
;
; 说明：使用了丘奇编码(https://en.wikipedia.org/wiki/Church_encoding)，从无到有构造解释器。

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; 布尔值
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define SHOWBOOL
  (lambda (b)
    (b #t #f)))

(define @true  (lambda (x y) x))
(define @false (lambda (x y) y))

(define NOT
  (lambda (bool)
    (bool @false @true)))

(define AND
  (lambda (boolx booly)
    (boolx booly boolx)))

(define OR
  (lambda (boolx booly)
    (boolx boolx booly)))

(define IS_ZERO
  (lambda (n)
    (n (lambda (x) @false) @true)))

(define IF
  (lambda (p x y)
    (p x y)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; 自然数
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define SHOWNUM
  (lambda (n)
    (n (lambda (x) (+ x 1)) 0)))

(define NUM_TO_LAMBDA
  (lambda (number)
    (cond ((= number 0) @0)
          (else (INC (NUM_TO_LAMBDA (- number 1)))))))

(define @0 (lambda (f a) a))

(define @1 (lambda (f a) (f a)))

(define INC
  (lambda (n)
    (lambda (f a)
      (f (n f a)))))

(define ADD
  (lambda (m n)
    (m INC n)))

;Curried-ADD - for function MUL
(define ADD-c
  (lambda (m)
    (lambda (n)
      (m INC n))))

(define MUL
  (lambda (m n)
    (n (ADD-c m) @0)))

;Curried-MUL - for function POW
(define MUL-c
  (lambda (m)
    (lambda (n)
      (n (ADD-c m) @0))))

(define POW
  (lambda (m n)
    (n (MUL-c m) @1)))

;some paticular numbers
(define @2 (lambda (f a) (f (f a))))
(define @3 (lambda (f a) (f (f (f a)))))
(define @4 (lambda (f a) (f (f (f (f a))))))
(define @5 (lambda (f a) (f (f (f (f (f a)))))))
(define @6 (lambda (f a) (f (f (f (f (f (f a))))))))
(define @7 (lambda (f a) (f (f (f (f (f (f (f a)))))))))
(define @8 (lambda (f a) (f (f (f (f (f (f (f (f a))))))))))
(define @9 (lambda (f a) (f (f (f (f (f (f (f (f (f a)))))))))))
(define @10 (lambda (f a) (f (f (f (f (f (f (f (f (f (f a))))))))))))
(define @11 (lambda (f a) (f (f (f (f (f (f (f (f (f (f (f a)))))))))))))
(define @12 (lambda (f a) (f (f (f (f (f (f (f (f (f (f (f (f a))))))))))))))
(define @13 (lambda (f a) (f (f (f (f (f (f (f (f (f (f (f (f (f a)))))))))))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; 有序对和减法
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define PAIR
  (lambda (x y)
    (lambda (f)
      (f x y))))

(define LEFT
  (lambda (pair)
    (pair @true)))

(define RIGHT
  (lambda (pair)
    (pair @false)))

;substraction
(define SLIDE
  (lambda (pair)
    (PAIR (RIGHT pair) (INC (RIGHT pair)))))

(define DEC
  (lambda (n)
    (LEFT (n SLIDE (PAIR @0 @0)))))

(define SUB
  (lambda (m n)
    (n DEC m)))

;comparation
(define IS_LE
  (lambda (num1 num2)
    (IS_ZERO (SUB num1 num2))))

(define IS_EQUAL
  (lambda (num1 num2)
    (AND (IS_LE num1 num2) (IS_LE num2 num1))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; Z组合子（Y组合子的应用序求值版本）
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;Y-Combinator
;注意：目标函数应使用单参形式
(define Y
  (lambda (S)
    ( (lambda (x) (S (lambda (y) ((x x) y))))
      (lambda (x) (S (lambda (y) ((x x) y)))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; 整数（暂时没有用）
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define INT
  (lambda (neg pos)
    (PAIR neg pos)))

(define *ZERO
  (PAIR @0 @0))

(define IS*ZERO
  (lambda (int)
    (AND (IS_ZERO (LEFT  int))
         (IS_ZERO (RIGHT int)))))

;整数标准化，也就是简化成至少一边为0的形式，这样就可以实现绝对值函数和符号函数了
(define *NORMALIZE
  (lambda (int)
    (IF (IS_LE (LEFT int) (RIGHT int))
        (INT @0 (SUB (RIGHT int) (LEFT int)))
        (INT (SUB (LEFT int) (RIGHT int)) @0))))

(define *ABS
  (lambda (int)
    (IF (IS_ZERO (LEFT (*NORMALIZE int)))
        (RIGHT (*NORMALIZE int))
        (LEFT  (*NORMALIZE int)))))

;@true +; @false -
(define *SGN
  (lambda (int)
    (IS_ZERO (LEFT (*NORMALIZE int)))))

(define SHOWINT
  (lambda (int)
    (cond ((SHOWBOOL (*SGN int)) (display "+") (SHOWNUM (*ABS int)))
          (else                  (display "-") (SHOWNUM (*ABS int))))))

(define *ADD
  (lambda (i j)
    (INT (ADD (LEFT  i) (LEFT  j))
         (ADD (RIGHT i) (RIGHT j)))))

(define *MUL
  (lambda (i j)
    (INT (ADD (MUL (LEFT i) (LEFT j)) (MUL (RIGHT i) (RIGHT j)))
         (ADD (MUL (LEFT i) (RIGHT j)) (MUL (RIGHT i) (LEFT j))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; 阶乘函数（组合子测试）
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;(display "7!=")
;(SHOWNUM 
;((Y (lambda (f)
;     (lambda (n)
;       (IF (IS_EQUAL n @0)
;           @1
;           (lambda (x y) ((MUL n (f (DEC n)))
;                          x
;                          y))
;       ))))
; @7)
;)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; 列表（二叉树）
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define NULL_LIST
  (PAIR @true @true))

(define IS_NULLLIST
  (lambda (list)
    (LEFT list)))

(define CONS
  (lambda (e l)
    (PAIR @false (PAIR e l))))

(define CAR
  (lambda (list)
    (LEFT (RIGHT list))))

(define CDR
  (lambda (list)
    (RIGHT (RIGHT list))))

(define COUNT
  (lambda (l)
    ((Y (lambda (f)
          (lambda (list)
            (IF (NOT (IS_NULLLIST list))
                (lambda (x y) ((INC (f (CDR list)))
                               x
                               y))
                @0))))
     l)))

;(display "Count(1,2,3,3,3)=")
;(SHOWNUM (COUNT (CONS @1 (CONS @2 (CONS @3 (CONS @3 (CONS @3 NULL_LIST)))))))

(define SHOWLIST
  (lambda (list)
    (cond ((SHOWBOOL (IS_NULLLIST list)) (display "N)"))
          (else (display (SHOWNUM (CAR list)))
                (display ",")
                (SHOWLIST (CDR list))))))

;(display "List=(")
;(SHOWLIST (CONS @1 (CONS @2 (CONS @3 (CONS @4 (CONS @5 NULL_LIST))))))
;(newline)

;闭区间
;注意Currying
(define RANGE
  (lambda (m n)
    (((Y (lambda (f)
          (lambda (a)
            (lambda (b)
            (IF (IS_LE a b)
                (lambda (z) ((CONS a ((f (INC a)) b))
                               z ))
                NULL_LIST
            )))))m)n)))

;(COUNT (RANGE @2 @4))
;(display "Range(2,7)=(")
;(SHOWLIST (RANGE @2 @7))
;(newline)


;高阶函数Fold和Map
(define FOLD
  (lambda (list init func)
    ((((Y (lambda (f)
          (lambda (l)
            (lambda (i)
              (lambda (g)
                (IF (IS_NULLLIST l)
                    i
                    (lambda (x y) (
                      (g (CAR l) (((f (CDR l)) i) g))
                      x y))
                ))))))list)init)func)))

(define MAP
  (lambda (list func)
    (((Y (lambda (f)
           (lambda (l)
             (lambda (g)
               (IF (IS_NULLLIST l)
                   NULL_LIST
                   (lambda (x) ((CONS (g (CAR l)) ((f (CDR l)) g)) x))
                )))))list)func)))

; 投影函数（常用）
(define PROJ
  (lambda (list index)
    ((((Y (lambda (f)
            (lambda (l)
              (lambda (i)
                (lambda (j)
                  (IF (IS_EQUAL i j)
                      (CAR l)
                      (lambda (x y) ((((f (CDR l)) i) (INC j)) x y))
                   ))))))list)index)@0)))

;(display "Fold(1:10,0,ADD)=")
;(SHOWNUM (FOLD (RANGE @1 @10) @0 ADD))

;(display "MAP(1:9,0,INC)=(")
;(SHOWLIST (MAP (RANGE @1 @9) INC))
;(newline)

;(display "Proj(2:10,5)=")
;(SHOWNUM (PROJ (MAP (RANGE @1 @9) INC) @5))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; 字符和字符串
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;ASCII('A')=65dec

(define $A (MUL @13 @5))
(define Alphabet (RANGE $A (ADD (ADD (ADD $A @10) @10) @5)))
(define $B (PROJ Alphabet @1))
(define $C (PROJ Alphabet @2))
(define $D (PROJ Alphabet @3))
(define $P (PROJ Alphabet @3))
(define $V (PROJ Alphabet (INC (MUL @10 @2)))) ;Variable Flag


;ASCII('a')=97dec
(define $a (ADD (MUL @10 @9) @7))
(define $b (INC $a))

(define SHOWCHAR
  (lambda (num)
    (integer->char (SHOWNUM num))))

(define CHAR_TO_LAMBDA
  (lambda (char)
    (NUM_TO_LAMBDA (char->integer char))))

(define SHOWSTR
  (lambda (cstr)
    (cond ((SHOWBOOL (IS_NULLLIST cstr)) (newline))
          (else (display (SHOWCHAR (CAR cstr))) (SHOWSTR (CDR cstr))))))

(define STR_TO_LAMBDA_ITER
  (lambda (str)
    (lambda (i)
      (cond ((= (+ i 1) (string-length str)) (CONS (CHAR_TO_LAMBDA (string-ref str i)) NULL_LIST))
            (else (CONS (CHAR_TO_LAMBDA (string-ref str i)) ((STR_TO_LAMBDA_ITER str) (+ i 1))))))))

(define STR_TO_LAMBDA
  (lambda (str)
    ((STR_TO_LAMBDA_ITER str) 0)))

;(display "SCharToCNum('a')=")
;(SHOWNUM (CHAR_TO_LAMBDA #\a))

;(display "CNumToSChar($A+@10)=")
;(SHOWCHAR (ADD $A @10))

;(display "ShowString=")
;(SHOWSTR (CONS $A (CONS $B (CONS $C (CONS $D (CONS $D (CONS $D NULL_LIST)))))))

;(display "StringToLambda=")
;(SHOWSTR (STR_TO_LAMBDA "Hello, λ-Calculus!"))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; λ演算的语法结构
;  暂时没有用到
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define Variable
  (lambda (char)
    (PAIR $V char)))

(define Parameter
  (lambda (char)
    (PAIR $P char)))

(define Defination
  (lambda (parameter body)
    (PAIR $D (PAIR (Parameter parameter) (PAIR $B body)))))

(define Application
  (lambda (left right)
    (PAIR $A (PAIR left right))))

(define Type
  (lambda (x)
    (LEFT x)))

(define GetChar
  (lambda varpara
    (RIGHT varpara)))

(define GetVarInDef
  (lambda (defination)
    (LEFT (RIGHT defination))))

(define GetBodyInDef
  (lambda (defination)
    (RIGHT (RIGHT defination))))

(define GetLeft
  (lambda (app)
    (LEFT (RIGHT app))))

(define GetRight
  (lambda (app)
    (RIGHT (RIGHT app))))

;SAMPLE=λa.λb.ab
(define Sample
  (Defination (Variable $a) (Defination (Variable $b) (Application (Variable $a) (Variable $b)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; 树形递归举例-斐波那契数
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define Fib
  (lambda (num)
    ((Y (lambda (f)
          (lambda (n)
            (IF (OR (IS_EQUAL n @0) (IS_EQUAL n @1))
                @1
                (lambda (x y) ((ADD (f (SUB n @1)) (f (SUB n @2))) x y))
            )))) num)))

;(SHOWNUM (Fib @6))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; Brainfuck解释器
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define @48 (MUL @12 @4))

; Brainfuck运行时环境示例
; 以字符0初始化
; 0位是数据指针DP，1位是程序指针CP，2位（地址0）开始是数据段，52位（地址50）开始是代码段
; BF逻辑地址称为offset，列表物理位置称为index
(define BF_ENV
  (CONS @0 (CONS @48 (STR_TO_LAMBDA "00000000000000000000000"))))

; 调试输出
(define BF_DEBUG
  (lambda (env)
    (newline)
    (display "== BrainFUCK DEBUG ===================================================")(newline)
    (display " DP = ")(display (SHOWNUM (CAR env)))(newline)
    (display " CP = ")(display (SHOWNUM (CAR (CDR env))))(newline)
    (display " LA : 0123456789ABCDEF0123456789ABCDEF0123456789ABCDEF0123456789ABCDEF")(newline)
    (display "MEM = ")
    (SHOWSTR (CDR (CDR env)))
    (display "======================================================================")(newline)
    (newline)
  ))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; 工具函数
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

; 左子列表 [0:index)
(define sub_L
  (lambda (index env)
    (((Y (lambda (f)
           (lambda (e)
             (lambda (iter)
               (IF (IS_EQUAL index iter)
                   NULL_LIST
                   (lambda (x) ((CONS (PROJ e iter) ((f e) (INC iter)))
                                x))
               ))))) env) @0)))

;(display "SUBL(")
;(SHOWLIST (sub_L @0 (STR_TO_LAMBDA "0123456789")))

; 右子列表 (index:N]
(define sub_R
  (lambda (index env)
    (((Y (lambda (f)
           (lambda (e)
             (lambda (iter)
               (IF (IS_ZERO iter)
                   e
                   (lambda (x) (((f (CDR e)) (DEC iter))
                                x))
                ))))) env) (INC index))))

;(display "SUBR(")
;(SHOWSTR (sub_R @0 (STR_TO_LAMBDA "0123456789")))

; 列表连接
(define list_catenate
  (lambda (_pre _post)
    (((Y (lambda (f)
           (lambda (pre)
             (lambda (post)
               (IF (IS_NULLLIST pre)
                   post
                   (lambda (x) ((CONS (CAR pre) ((f (CDR pre)) post))
                                x))
               ))))) _pre) _post)))

;(display "(")
;(SHOWSTR (list_catenate (STR_TO_LAMBDA "0123") (STR_TO_LAMBDA "456789")))


; 计算数据指针的物理地址index
(define data_index
  (lambda (env)
    (ADD @2 (CAR env))))

; 读取当前指针指向的cell值
(define read_data
  (lambda (env)
    (PROJ env (data_index env))))

;(display "read_data=")
;(SHOWNUM (read_data (CONS @1 (CONS @2 (STR_TO_LAMBDA "000000000")))))

; 修改当前指针指向的cell值
; 注意：传入单参函数
(define modify_data
  (lambda (func env)
    (list_catenate (sub_L (data_index env) env)
                   (CONS (func (read_data env))
                         (sub_R (data_index env) env)))))

;(display "modify_data=")
;(BF_DEBUG (modify_data (ADD-c @5) (CONS @5 (CONS @2 (STR_TO_LAMBDA "000000000")))))

; 计算程序指针的物理地址index
(define code_index
  (lambda (env)
    (ADD @2 (CAR (CDR env)))))

; 取CP指向的指令码
(define read_code
  (lambda (env)
    (PROJ env (code_index env))))

;(display "read_code=")
;(SHOWCHAR (read_code (CONS @1 (CONS @3 (STR_TO_LAMBDA "000C00000")))))

; 计算当前指令逻辑地址（offset）左侧的**匹配**的“[”指令的所在物理地址（index）
; 这里计算的是（当前所在内层）循环的入口地址
(define ret_index
  (lambda (env)
    ((((Y (lambda (f)
            (lambda (e)
              (lambda (cindex)
                (lambda (flag)
                  (IF (IS_EQUAL (PROJ env cindex) (CHAR_TO_LAMBDA #\]))
                      (lambda (x y) ((((f env) (DEC cindex)) (INC flag)) x y))
                      (lambda (x y) (
                         (IF (IS_EQUAL (PROJ env cindex) (CHAR_TO_LAMBDA #\[))
                             (lambda (x y) (
                                (IF (IS_ZERO flag)
                                    cindex
                                    (lambda (x y) ((((f env) (DEC cindex)) (DEC flag)) x y))
                                ) x y))
                             (lambda (x y) (
                                (((f env) (DEC cindex)) flag) x y))
                          ) x y))
                  )))))) env) (DEC (code_index env))) @0)))


;(display "ret[_index=")
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;0123456789ABCD(index)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;0123456789AB(offset)
;(SHOWNUM (ret_index (CONS @0 (CONS @7 (STR_TO_LAMBDA "[[[[[][]A]]]]1234")))))

; 计算当前指令逻辑地址（offset）右侧的**匹配**的“]”指令的所在物理地址（index）的后一位
; 这里计算的是（当前所在内层）循环的跳出地址
(define pass_index
  (lambda (env)
    ((((Y (lambda (f)
            (lambda (e)
              (lambda (cindex)
                (lambda (flag)
                  (IF (IS_EQUAL (PROJ env cindex) (CHAR_TO_LAMBDA #\[))
                      (lambda (x y) ((((f env) (INC cindex)) (INC flag)) x y))
                      (lambda (x y) (
                         (IF (IS_EQUAL (PROJ env cindex) (CHAR_TO_LAMBDA #\]))
                             (lambda (x y) (
                                (IF (IS_ZERO flag)
                                    (INC cindex)
                                    (lambda (x y) ((((f env) (INC cindex)) (DEC flag)) x y))
                                ) x y))
                             (lambda (x y) (
                                (((f env) (INC cindex)) flag) x y))
                          ) x y))
                  )))))) env) (INC (code_index env))) @0)))

;(display "pass]_index=")
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;0123456789ABCDEF(index)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;0123456789ABCDEF(offset)
;(SHOWNUM (pass_index (CONS @0 (CONS @3 (STR_TO_LAMBDA "0[A[A]A[A]A]]]]1234")))))

; 修改指令指针（下条指令逻辑地址）
(define modify_code_offset_0
  (lambda (coffset env)
    (list_catenate (sub_L @1 env)
                   (CONS coffset
                         (sub_R @1 env)))))

; 修改指令指针（简化版）
(define modify_code_offset
  (lambda (coffset env)
    (CONS (CAR env) (CONS coffset (sub_R @1 env)))))

;(display "modify_code_offset+2!!!=")
;(SHOWNUM (code_index (modify_code_offset @10 
;                                         (CONS @0 (CONS @7 (STR_TO_LAMBDA "[[[[[][]A]]]]1234")))
;                                         )))

; 获取下一条指令的逻辑地址
(define next
  (lambda (env)
    (INC (CAR (CDR env)))))

; CP加一
(define cp++
  (lambda (env)
    (modify_code_offset (INC (CAR (CDR env))) env)))

;(BF_DEBUG (cp++ (CONS @0 (CONS @5 (STR_TO_LAMBDA "12000[->+<]  ")))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; 指令语义
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
             
; Note: Every lambda function representing a Brainfuck instruction
;       receives an OLD ENV and returns a NEW ENV.
(define >
  (lambda (env)
    (cp++ (CONS (INC (CAR env)) (CDR env)))))

(define <
  (lambda (env)
    (cp++ (CONS (DEC (CAR env)) (CDR env)))))

(define ++
  (lambda (env)
    (cp++ (modify_data INC env))))

(define --
  (lambda (env)
    (cp++ (modify_data DEC env))))

; .
(define o
  (lambda (env)
    (cp++ (SHOWCHAR (PROJ env (INC (CAR env)))))))

; ,暂不实现

;(BF_DEBUG (> (CONS @0 (CONS @5 (STR_TO_LAMBDA "12000[->+<]  ")))))

; [
(define loopl
  (lambda (env)
    (IF (IS_EQUAL (CHAR_TO_LAMBDA #\0) (read_data env))
        (lambda (x) ((modify_code_offset (SUB (pass_index env) @2) env) x)) ;直接跳出
        (lambda (x) ((cp++ env) x)) ;下条指令
    )))

;(display "loopl=")
;(SHOWLIST (loopl (CONS @0 (CONS @3 (STR_TO_LAMBDA "1[A[A]A[A]A]]]]1234")))))

; ]
(define loopr
  (lambda (env)
    (modify_code_offset (SUB (ret_index env) @2) env)))

;(display "loopr=")
;(SHOWLIST (loopr (CONS @0 (CONS @5 (STR_TO_LAMBDA "1[A[A]A[A]A]]]]1234")))))


; 单步执行：执行当前CP指向的指令
; 执行的结果当然是保存在新的env里面啦
(define step
  (lambda (env)
    (IF (IS_EQUAL (read_code env) (CHAR_TO_LAMBDA #\+))
        (lambda (x) ((++ env) x))
        (lambda (x) (
           (IF (IS_EQUAL (read_code env) (CHAR_TO_LAMBDA #\-))
               (lambda (x) ((-- env) x))
               (lambda (x) (
                  (IF (IS_EQUAL (read_code env) (CHAR_TO_LAMBDA #\>))
                      (lambda (x) ((> env) x))
                      (lambda (x) (
                         (IF (IS_EQUAL (read_code env) (CHAR_TO_LAMBDA #\<))
                             (lambda (x) ((< env) x))
                             (lambda (x) (
                                (IF (IS_EQUAL (read_code env) (CHAR_TO_LAMBDA #\o))
                                    (lambda (x) ((o env) x))
                                    (lambda (x) (
                                       (IF (IS_EQUAL (read_code env) (CHAR_TO_LAMBDA #\i))
                                           (lambda (x) ((o env) x)) ;暂未实现
                                           (lambda (x) (
                                              (IF (IS_EQUAL (read_code env) (CHAR_TO_LAMBDA #\[))
                                                  (lambda (x) ((loopl env) x))
                                                  (lambda (x) (
                                                     (IF (IS_EQUAL (read_code env) (CHAR_TO_LAMBDA #\]))
                                                         (lambda (x) ((loopr env) x))
                                                         env ; 未知指令，不执行任何动作
                                                     ) x))
                                              ) x))
                                       ) x))
                                ) x))
                         ) x))
                  ) x))
           ) x))
     )))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; 执行加速
;   避免对过长的表达式求值
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define to_string
  (lambda (env)
    (cond ((SHOWBOOL (IS_NULLLIST env)) "")
          (else (string-append (make-string 1 (SHOWCHAR (CAR env))) (to_string (CDR env)))))))

(define STEPx
  (lambda (prev_env)
    (step (CONS (CAR prev_env) (CONS (CAR (CDR prev_env)) (STR_TO_LAMBDA (to_string (CDR (CDR prev_env)))))))))

(define ENVx
  (lambda (stepx)
    (CONS (CAR stepx) (CONS (CAR (CDR stepx)) (STR_TO_LAMBDA (to_string (CDR (CDR stepx))))))))

(define ITER
  (lambda (env)
    (ENVx (STEPx env))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; 解释器主体
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

; Brainfuck初始环境，含数据初值和代码
; 程序意义：计算逻辑地址0和1位置两个数字的和，并将结果保存在1位置。
;                 DP       CP                 MEMORY(TAPE)
(define env (CONS @0 (CONS @2 (STR_TO_LAMBDA "23[->+<]$"))))

; 解释器
;   读取到$字符时停止，并输出调试信息
(define bf_interpreter
  (lambda (env)
    (cond ((SHOWBOOL (IS_EQUAL (read_code env) (CHAR_TO_LAMBDA #\$))) (BF_DEBUG env))
          (else (bf_interpreter (ITER env))))))

; 开始解释执行
(bf_interpreter env)
