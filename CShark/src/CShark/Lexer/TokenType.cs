// Copyright: Hemanth Kapila (2016).
// See the LICENSE file in the project root for more information.

using System;

namespace CShark.Lexer
{
    public enum TokenType
    {
        Identifier,
        MuIdentifier,

        IntConstant,
        LongConstant,
        ShortConstant,
        FloatConstant,
        DoubleConstant,
        UIntConstant,
        ULongConstant,
        UShortConstant,

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

        Ignorable
    }
}
