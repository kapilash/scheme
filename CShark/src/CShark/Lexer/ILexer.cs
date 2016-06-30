// Copyright: Hemanth Kapila (2016).
// See the LICENSE file in the project root for more information.

using System;
namespace CShark.Lexer
{
    internal interface ILexer
    {
        Token Scan(IReader reader);
    }
}
