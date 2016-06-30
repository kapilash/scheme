using System;

namespace MuScheme.Lexer
{
    public enum TokenType
    {
        Variable,
        MuVariable,

        IntConstant,
        LongConstant,
        ShortConstant,
        FloatConstant,
        DoubleConstant,
        ByteConstant,
        UIntConstant,
        ULongConstant,
        UShortConstant,
        UByteConstant,
        ComplexConstant,

        CharConstant,
        StringConstant,

        TrueConst,
        FalseConst,
        NullConst,

        MuFunKeyword,
        MuMacroKeyword,
        FuncKeyword,
        ProcKeyword,
        IfKeyword,
        ThenKeyword,
        ElseKeyword,
        CaseKeyword,
        OfKeyword,
        MuStructKeyword,
        WhileKeyword,
        ForKeyword,
        BreakKeyword,
        LetKeyword,
        QuoteKeyword,
        LambdaKeyword,

        CurlyBraceOpen,
        CurlyBraceClose,
        SquareBraceOpen,
        SquareBraceClose,
        BraceOpen,
        BraceClose,
        Assign,
        Dot,
        SemiColon,
        
    }
}
