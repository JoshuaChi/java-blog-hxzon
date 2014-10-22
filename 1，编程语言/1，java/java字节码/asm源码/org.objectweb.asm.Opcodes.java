//org.objectweb.asm.Opcodes.java
//注释by hxzon

/***
 * ASM: a very small and fast Java bytecode manipulation framework
 * Copyright (c) 2000-2011 INRIA, France Telecom
 * All rights reserved.
 *
 * Redistribution and use in source and binary forms, with or without
 * modification, are permitted provided that the following conditions
 * are met:
 * 1. Redistributions of source code must retain the above copyright
 *    notice, this list of conditions and the following disclaimer.
 * 2. Redistributions in binary form must reproduce the above copyright
 *    notice, this list of conditions and the following disclaimer in the
 *    documentation and/or other materials provided with the distribution.
 * 3. Neither the name of the copyright holders nor the names of its
 *    contributors may be used to endorse or promote products derived from
 *    this software without specific prior written permission.
 *
 * THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS IS"
 * AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE
 * IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE
 * ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT OWNER OR CONTRIBUTORS BE
 * LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR
 * CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF
 * SUBSTITUTE GOODS OR SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS
 * INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN
 * CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE)
 * ARISING IN ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF
 * THE POSSIBILITY OF SUCH DAMAGE.
 */
package org.objectweb.asm;

/**
 * Defines the JVM opcodes, access flags and array type codes. This interface
 * does not define all the JVM opcodes because some opcodes are automatically
 * handled. For example, the xLOAD and xSTORE opcodes are automatically replaced
 * by xLOAD_n and xSTORE_n opcodes when possible. The xLOAD_n and xSTORE_n
 * opcodes are therefore not defined in this interface. Likewise for LDC,
 * automatically replaced by LDC_W or LDC2_W when necessary, WIDE, GOTO_W and
 * JSR_W.
 * 
 * @author Eric Bruneton
 * @author Eugene Kuleshov
 */
public interface Opcodes {

    // ASM API versions

    int ASM4 = 4 << 16 | 0 << 8 | 0;
    int ASM5 = 5 << 16 | 0 << 8 | 0;

    // versions

    int V1_1 = 3 << 16 | 45;
    int V1_2 = 0 << 16 | 46;
    int V1_3 = 0 << 16 | 47;
    int V1_4 = 0 << 16 | 48;
    int V1_5 = 0 << 16 | 49;
    int V1_6 = 0 << 16 | 50;
    int V1_7 = 0 << 16 | 51;
    int V1_8 = 0 << 16 | 52;

    // access flags 访问标记

    int ACC_PUBLIC = 0x0001; // class, field, method
    int ACC_PRIVATE = 0x0002; // class, field, method
    int ACC_PROTECTED = 0x0004; // class, field, method
    int ACC_STATIC = 0x0008; // field, method
    int ACC_FINAL = 0x0010; // class, field, method, parameter
    int ACC_SUPER = 0x0020; // class
    int ACC_SYNCHRONIZED = 0x0020; // method
    int ACC_VOLATILE = 0x0040; // field
    int ACC_BRIDGE = 0x0040; // method
    int ACC_VARARGS = 0x0080; // method
    int ACC_TRANSIENT = 0x0080; // field
    int ACC_NATIVE = 0x0100; // method
    int ACC_INTERFACE = 0x0200; // class
    int ACC_ABSTRACT = 0x0400; // class, method
    int ACC_STRICT = 0x0800; // method
    int ACC_SYNTHETIC = 0x1000; // class, field, method, parameter
    int ACC_ANNOTATION = 0x2000; // class
    int ACC_ENUM = 0x4000; // class(?) field inner
    int ACC_MANDATED = 0x8000; // parameter

    // ASM specific pseudo access flags

    int ACC_DEPRECATED = 0x20000; // class, field, method

    // types for NEWARRAY 新建数组的元素类型

    int T_BOOLEAN = 4;
    int T_CHAR = 5;
    int T_FLOAT = 6;
    int T_DOUBLE = 7;
    int T_BYTE = 8;
    int T_SHORT = 9;
    int T_INT = 10;
    int T_LONG = 11;

    // tags for Handle

    int H_GETFIELD = 1;//访问实例字段
    int H_GETSTATIC = 2;//访问静态字段
    int H_PUTFIELD = 3;//设置实例字段
    int H_PUTSTATIC = 4;//设置静态字段
    int H_INVOKEVIRTUAL = 5;//虚拟调用
    int H_INVOKESTATIC = 6;//调用静态方法
    int H_INVOKESPECIAL = 7;//特殊调用
    int H_NEWINVOKESPECIAL = 8;//创建实例调用
    int H_INVOKEINTERFACE = 9;//接口方法调用

    // stack map frame types

    /**
     * Represents an expanded frame. See {@link ClassReader#EXPAND_FRAMES}.
     */
    int F_NEW = -1;

    /**
     * Represents a compressed frame with complete frame data.
     */
    int F_FULL = 0;

    /**
     * Represents a compressed frame where locals are the same as the locals in
     * the previous frame, except that additional 1-3 locals are defined, and
     * with an empty stack.
     */
    int F_APPEND = 1;

    /**
     * Represents a compressed frame where locals are the same as the locals in
     * the previous frame, except that the last 1-3 locals are absent and with
     * an empty stack.
     */
    int F_CHOP = 2;

    /**
     * Represents a compressed frame with exactly the same locals as the
     * previous frame and with an empty stack.
     */
    int F_SAME = 3;

    /**
     * Represents a compressed frame with exactly the same locals as the
     * previous frame and with a single value on the stack.
     */
    int F_SAME1 = 4;

    Integer TOP = new Integer(0);
    Integer INTEGER = new Integer(1);
    Integer FLOAT = new Integer(2);
    Integer DOUBLE = new Integer(3);
    Integer LONG = new Integer(4);
    Integer NULL = new Integer(5);
    Integer UNINITIALIZED_THIS = new Integer(6);

    // opcodes // visit method (- = idem)

    int NOP = 0; // visitInsn
    int ACONST_NULL = 1; // -
    int ICONST_M1 = 2; // -
    int ICONST_0 = 3; // -
    int ICONST_1 = 4; // -
    int ICONST_2 = 5; // -
    int ICONST_3 = 6; // -
    int ICONST_4 = 7; // -
    int ICONST_5 = 8; // -
    int LCONST_0 = 9; // -
    int LCONST_1 = 10; // -
    int FCONST_0 = 11; // -
    int FCONST_1 = 12; // -
    int FCONST_2 = 13; // -
    int DCONST_0 = 14; // -
    int DCONST_1 = 15; // -
    int BIPUSH = 16; // visitIntInsn
    int SIPUSH = 17; // -
    int LDC = 18; // visitLdcInsn  将int, float或String型常量值从常量池中推送至栈顶
    // int LDC_W = 19; // -  将int, float或String型常量值从常量池中推送至栈顶（宽索引）
    // int LDC2_W = 20; // -  将long或double型常量值从常量池中推送至栈顶（宽索引）
    int ILOAD = 21; // visitVarInsn  将指定的int型本地变量推送至栈顶
    int LLOAD = 22; // -  long型
    int FLOAD = 23; // -  float型
    int DLOAD = 24; // -  double型
    int ALOAD = 25; // -  将指定的引用类型本地变量推送至栈顶
    // int ILOAD_0 = 26; // -  将第一个int型本地变量推送至栈顶
    // int ILOAD_1 = 27; // -  将第二个int型本地变量推送至栈顶
    // int ILOAD_2 = 28; // -  将第三个int型本地变量推送至栈顶
    // int ILOAD_3 = 29; // -
    // int LLOAD_0 = 30; // -
    // int LLOAD_1 = 31; // -
    // int LLOAD_2 = 32; // -
    // int LLOAD_3 = 33; // -
    // int FLOAD_0 = 34; // -
    // int FLOAD_1 = 35; // -
    // int FLOAD_2 = 36; // -
    // int FLOAD_3 = 37; // -
    // int DLOAD_0 = 38; // -
    // int DLOAD_1 = 39; // -
    // int DLOAD_2 = 40; // -
    // int DLOAD_3 = 41; // -
    // int ALOAD_0 = 42; // -
    // int ALOAD_1 = 43; // -
    // int ALOAD_2 = 44; // -
    // int ALOAD_3 = 45; // -
    int IALOAD = 46; // visitInsn  将int型数组指定索引的值推送至栈顶
    int LALOAD = 47; // -
    int FALOAD = 48; // -
    int DALOAD = 49; // -
    int AALOAD = 50; // -  将引用型数组指定索引的值推送至栈顶
    int BALOAD = 51; // -  将boolean或byte型数组指定索引的值推送至栈顶
    int CALOAD = 52; // -  将char型数组指定索引的值推送至栈顶
    int SALOAD = 53; // -  将short型数组指定索引的值推送至栈顶
    int ISTORE = 54; // visitVarInsn
    int LSTORE = 55; // -
    int FSTORE = 56; // -
    int DSTORE = 57; // -
    int ASTORE = 58; // -
    // int ISTORE_0 = 59; // -
    // int ISTORE_1 = 60; // -
    // int ISTORE_2 = 61; // -
    // int ISTORE_3 = 62; // -
    // int LSTORE_0 = 63; // -
    // int LSTORE_1 = 64; // -
    // int LSTORE_2 = 65; // -
    // int LSTORE_3 = 66; // -
    // int FSTORE_0 = 67; // -
    // int FSTORE_1 = 68; // -
    // int FSTORE_2 = 69; // -
    // int FSTORE_3 = 70; // -
    // int DSTORE_0 = 71; // -
    // int DSTORE_1 = 72; // -
    // int DSTORE_2 = 73; // -
    // int DSTORE_3 = 74; // -
    // int ASTORE_0 = 75; // -
    // int ASTORE_1 = 76; // -
    // int ASTORE_2 = 77; // -
    // int ASTORE_3 = 78; // -
    int IASTORE = 79; // visitInsn
    int LASTORE = 80; // -
    int FASTORE = 81; // -
    int DASTORE = 82; // -
    int AASTORE = 83; // -
    int BASTORE = 84; // -
    int CASTORE = 85; // -
    int SASTORE = 86; // -
    int POP = 87; // -
    int POP2 = 88; // -
    int DUP = 89; // -
    int DUP_X1 = 90; // -
    int DUP_X2 = 91; // -
    int DUP2 = 92; // -
    int DUP2_X1 = 93; // -
    int DUP2_X2 = 94; // -
    int SWAP = 95; // -
    int IADD = 96; // - int加
    int LADD = 97; // - long加
    int FADD = 98; // -
    int DADD = 99; // -
    int ISUB = 100; // - int减
    int LSUB = 101; // -
    int FSUB = 102; // -
    int DSUB = 103; // -
    int IMUL = 104; // -
    int LMUL = 105; // -
    int FMUL = 106; // -
    int DMUL = 107; // -
    int IDIV = 108; // -
    int LDIV = 109; // -
    int FDIV = 110; // -
    int DDIV = 111; // -
    int IREM = 112; // -
    int LREM = 113; // -
    int FREM = 114; // -
    int DREM = 115; // -
    int INEG = 116; // -
    int LNEG = 117; // -
    int FNEG = 118; // -
    int DNEG = 119; // -
    int ISHL = 120; // -
    int LSHL = 121; // -
    int ISHR = 122; // -
    int LSHR = 123; // -
    int IUSHR = 124; // -
    int LUSHR = 125; // -
    int IAND = 126; // -
    int LAND = 127; // -
    int IOR = 128; // -
    int LOR = 129; // -
    int IXOR = 130; // -
    int LXOR = 131; // -
    int IINC = 132; // visitIincInsn  int自增指令
    int I2L = 133; // visitInsn  将栈顶int型数值强制转换成long型数值，并将结果压入栈顶
    int I2F = 134; // -
    int I2D = 135; // -
    int L2I = 136; // -
    int L2F = 137; // -
    int L2D = 138; // -
    int F2I = 139; // -
    int F2L = 140; // -
    int F2D = 141; // -
    int D2I = 142; // -
    int D2L = 143; // -
    int D2F = 144; // -
    int I2B = 145; // -
    int I2C = 146; // -
    int I2S = 147; // -
    int LCMP = 148; // - 比较栈顶两long型数值大小，并将结果（1，0，-1）压入栈顶
    int FCMPL = 149; // - 比较栈顶两float型数值大小，并将结果（1，0，-1）压入栈顶；当其中一个数值为NaN时，将-1压入栈顶
    int FCMPG = 150; // - 比较栈顶两float型数值大小，并将结果（1，0，-1）压入栈顶；当其中一个数值为NaN时，将1压入栈顶
    int DCMPL = 151; // - 比较栈顶两double型数值大小，并将结果（1，0，-1）压入栈顶；当其中一个数值为NaN时，将-1压入栈顶
    int DCMPG = 152; // - 比较栈顶两double型数值大小，并将结果（1，0，-1）压入栈顶；当其中一个数值为NaN时，将1压入栈顶
    int IFEQ = 153; // visitJumpInsn  当栈顶int型数值等于0时跳转
    int IFNE = 154; // -  当栈顶int型数值不等于0时跳转
    int IFLT = 155; // -  当栈顶int型数值小于0时跳转
    int IFGE = 156; // -  当栈顶int型数值大于等于0时跳转
    int IFGT = 157; // -
    int IFLE = 158; // -
    int IF_ICMPEQ = 159; // -
    int IF_ICMPNE = 160; // -
    int IF_ICMPLT = 161; // -
    int IF_ICMPGE = 162; // -
    int IF_ICMPGT = 163; // -
    int IF_ICMPLE = 164; // -
    int IF_ACMPEQ = 165; // -
    int IF_ACMPNE = 166; // -
    int GOTO = 167; // -
    int JSR = 168; // -
    int RET = 169; // visitVarInsn
    int TABLESWITCH = 170; // visiTableSwitchInsn  表跳转（跳转表）
    int LOOKUPSWITCH = 171; // visitLookupSwitch
    int IRETURN = 172; // visitInsn 返回int
    int LRETURN = 173; // - 返回long
    int FRETURN = 174; // -
    int DRETURN = 175; // -
    int ARETURN = 176; // -
    int RETURN = 177; // -
    int GETSTATIC = 178; // visitFieldInsn 访问静态字段
    int PUTSTATIC = 179; // -
    int GETFIELD = 180; // -
    int PUTFIELD = 181; // -
    int INVOKEVIRTUAL = 182; // visitMethodInsn
    int INVOKESPECIAL = 183; // -
    int INVOKESTATIC = 184; // -
    int INVOKEINTERFACE = 185; // -
    int INVOKEDYNAMIC = 186; // visitInvokeDynamicInsn  动态调用
    int NEW = 187; // visitTypeInsn
    int NEWARRAY = 188; // visitIntInsn  创建原始类型的数组
    int ANEWARRAY = 189; // visitTypeInsn  创建引用类型的数组
    int ARRAYLENGTH = 190; // visitInsn 获取数组长度
    int ATHROW = 191; // -
    int CHECKCAST = 192; // visitTypeInsn
    int INSTANCEOF = 193; // -
    int MONITORENTER = 194; // visitInsn 进入监视器
    int MONITOREXIT = 195; // - 离开监视器
    // int WIDE = 196; // NOT VISITED
    int MULTIANEWARRAY = 197; // visitMultiANewArrayInsn  创建多维度数组
    int IFNULL = 198; // visitJumpInsn  null判断（null跳转）
    int IFNONNULL = 199; // - 非null判断
    // int GOTO_W = 200; // -
    // int JSR_W = 201; // -
}

