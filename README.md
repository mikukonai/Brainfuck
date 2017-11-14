# Brainfuck解释器

使用Racket实现的简单Brainfuck解释器。  
A simple Brainfuck interpreter written in Racket Scheme.

Brainfuck名副其实，是一门很奇葩的语言。它的行为几乎完全模仿图灵机，因此是图灵完备的。关于Brainfuck的更多细节，请参考[维基百科](https://en.wikipedia.org/wiki/Brainfuck)。

这段代码本来是λ演算的学习笔记，后来为了证明λ演算的图灵完备性，干脆从[丘奇编码](https://en.wikipedia.org/wiki/Church_encoding)出发，实现了一个Brainfuck解释器。

实际上只需要实现[μ-递归函数](https://en.wikipedia.org/wiki/%CE%9C-recursive_function)，就可以证明λ演算的图灵完备性。这一点，在《可计算性与计算复杂性导论》的笔记中，已经写过了。

这东西没什么使用价值，纯粹是做着玩的。

为提高运行效率，使用原生Racket重写了解释器部分。新文件为`brainfuck-scheme.rkt`。

## 运行环境

请务必使用[DrRacket](http://racket-lang.org/)运行此程序。

## 存在的问题

最初的`brainfuck.rkt`使用了丘奇编码，因此运行时环境在执行步骤间的传递实际上是以闭包形式传递，运行效率极低；并且存在大量重复求值，很难处理具有副作用的指令（即`.`和`,`两个指令）。

因此使用原生Racket重写了解释器部分（但Y组合子没有移除），运行效率大幅提高。

没有实现`,`指令，因为觉得用处不大，不能说明问题本质。

## 展望

目前还很不完善，也不打算完善。GitHub上有其他人做过BF可视化的工作，鉴于Racket具有比较易用的绘图功能，可以考虑做一下。

## 权利声明

游戏之作，仅供参考。作者对此代码不保留任何权利，亦不必承担任何责任。

