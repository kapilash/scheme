﻿// Copyright: Hemanth Kapila (2016).
// See the LICENSE file in the project root for more information.

using System;
using System.Text;
using System.Diagnostics;

namespace CShark.Lexer
{
    static class NumberUtils
    {
        internal static void AppendTill(IReader reader, Predicate<char> predicate, StringBuilder builder)
        {
            do
            {
                if (predicate(reader.Current))
                {
                    builder.Append(reader.Current);
                }
                else
                {
                    reader.Revert(reader.Current);
                    return;
                }
            } while (reader.MoveNext());
        }

        /// <summary>
        ///   mantissa is of the form [0-9]*([eE][+-]?[0-9]+)?
        /// </summary>
        internal static double ReadMantissa(IReader reader, string prefix)
        {
            var builder = new StringBuilder();
            builder.Append(prefix);
            builder.Append('.');

            AppendTill(reader, (c) => c >= '0' && c <= '9', builder);
            if (reader.MoveNext())
            {
                if (reader.Current == 'e' || reader.Current == 'E')
                {
                    builder.Append('E');
                    if (reader.MoveNext())
                    {
                        if (reader.Current == '+' || reader.Current == '-')
                        {
                            builder.Append(reader.Current);
                            if (reader.MoveNext())
                            {
                                AppendTill(reader, (c) => c >= '0' && c <= '9', builder);
                            }
                        }
                        else
                        {
                            AppendTill(reader, (c) => c >= '0' && c <= '9', builder);
                        }
                    }
                }
                else
                {
                    AppendTill(reader, (c) => c >= '0' && c <= '9', builder);
                }
            }

            return Convert.ToDouble(builder.ToString());
        }

        internal static Token ContinueWithSuffix(IReader reader, int fromBase, int line, int column, string text)
        {
            switch (reader.Current)
            {
                case 'd':
                case 'D':
                    {
                        return new Token(TokenType.DoubleConstant, line, column, Convert.ToDouble(text));
                    }

                case 'f':
                case 'F':
                    {
                        return new Token(TokenType.FloatConstant, line, column, Convert.ToSingle(text));
                    }

                case 'l':
                case 'L':
                    {
                        if (reader.MoveNext())
                        {
                            if (reader.Current == 'u' || reader.Current == 'U')
                            {
                                return new Token(TokenType.ULongConstant, line, column, Convert.ToUInt64(text, fromBase));
                            }

                            reader.Revert(reader.Current);
                        }

                        return new Token(TokenType.LongConstant, line, column, Convert.ToInt64(text, fromBase));
                    }

                case 's':
                case 'S':
                    {
                        if (reader.MoveNext())
                        {
                            if (reader.Current == 'u' || reader.Current == 'U')
                            {
                                return new Token(TokenType.UShortConstant, line, column, Convert.ToUInt16(text, fromBase));
                            }

                            reader.Revert(reader.Current);
                        }

                        return new Token(TokenType.ShortConstant, line, column, Convert.ToInt16(text, fromBase));
                    }

                case 'u':
                case 'U':
                    {

                        if (reader.MoveNext())
                        {
			    if (reader.Current == 'l' || reader.Current == 'L')
			    {
                                return new Token(TokenType.ULongConstant, line, column, Convert.ToUInt64(text, fromBase));
                            }

                            if (reader.Current == 's' || reader.Current == 'S')
                            {
                                return new Token(TokenType.UShortConstant, line, column, Convert.ToUInt16(text, fromBase));
                            }

                            if (reader.Current == 'y' || reader.Current == 'Y')
                            {
                                return new Token(TokenType.ByteConstant, line, column, Convert.ToByte(text, fromBase));
                            }

                            reader.Revert(reader.Current);
                        }

                        return new Token(TokenType.UIntConstant, line, column, Convert.ToUInt32(text, fromBase));
                    }

                case 'y':
                case 'Y':
                    {
                        if (reader.MoveNext())
                        {
                            if (reader.Current == 'u' || reader.Current == 'U')
                            {
                                return new Token(TokenType.ByteConstant, line, column, Convert.ToByte(text, fromBase));
                            }

                            reader.Revert(reader.Current);
                        }

                        return new Token(TokenType.SByteConstant, line, column, Convert.ToSByte(text, fromBase));
                    }

                default:
                    {
                        reader.Revert(reader.Current);
                        break;
                    }
            }

            return new Lexer.Token(TokenType.IntConstant, line, column, Convert.ToInt32(text, fromBase));
        }

        internal static Token ScanDouble(IReader reader, int line, int column, string text)
        {
            double dbl = 0d;
            try
            {
                dbl = NumberUtils.ReadMantissa(reader, text);
            }
            catch (FormatException f)
            {
                throw new ScannerException(f.Message, line, column);
            }

            if (reader.MoveNext())
            {
                if (reader.Current == 'f' || reader.Current == 'F')
                {
                    try
                    {
                        float f = Convert.ToSingle(dbl);
                        return new Token(TokenType.FloatConstant, line, column, Convert.ToSingle(dbl));
                    }
                    catch (FormatException f)
                    {
                        throw new ScannerException(f.Message, line, column);
                    }
                }
                if (reader.Current != 'd' && reader.Current != 'D')
                {
                    reader.Revert(reader.Current);
                }
            }

            return new Token(TokenType.DoubleConstant, line, column, dbl);
        }
    }

    internal class ZeroLexer : ILexer
    {
        public Token Scan(IReader reader)
        {
            int line = reader.Line;
            int column = reader.Column;
            if (reader.MoveNext())
            {
                if (reader.Current == '.')
                {
                    return NumberUtils.ScanDouble(reader, line, column, "0");
                }
                else if (reader.Current == 'x' || reader.Current == 'X')
                {
                    if (!reader.MoveNext())
                    {
                        throw new ScannerException("Unexpected EOF", line, column);
                    }

                    var builder = new StringBuilder();
                    NumberUtils.AppendTill(reader, (c) => (c >= '0' && c <= '9') || (c >= 'A' && c <= 'F') || (c >= 'a' && c <= 'f'), builder);
                    if (reader.MoveNext())
                    {
                        try
                        {
                            return NumberUtils.ContinueWithSuffix(reader, 16, line, column, builder.ToString());
                        }
                        catch (FormatException f)
                        {
                            throw new ScannerException(f.Message, line, column);
                        }
                    }

                    return new Token(TokenType.IntConstant, line, column, Convert.ToInt32(builder.ToString(), 16));
                }
                else
                {
                    reader.Revert(reader.Current);
                }
            }

            return new Token(TokenType.IntConstant, line, column, 0);
        }
    }

    internal class NumberLexer : ILexer
    {
        public virtual Token Scan(IReader reader)
        {
            int line = reader.Line;
            int column = reader.Column;
            var builder = new StringBuilder();
            NumberUtils.AppendTill(reader, (c) => c >= '0' && c <= '9', builder);
            if (reader.MoveNext())
            {
                if (reader.Current == '.')
                {
                    if (reader.MoveNext())
                    {
                        try
                        {
                            return NumberUtils.ScanDouble(reader, line, column, builder.ToString());
                        }
                        catch(FormatException f)
                        {
                            throw new ScannerException(f.Message, line, column);
                        }
                    }

                    try
                    {
                        return new Token(TokenType.DoubleConstant, line, column, Convert.ToDouble(builder.ToString()));
                    }
                    catch(FormatException f)
                    {
                        throw new ScannerException(f.Message, line, column);
                    }
                }
                try
                {
                    return NumberUtils.ContinueWithSuffix(reader, 10, line, column, builder.ToString());
                }
                catch(FormatException f)
                {
                    throw new ScannerException(f.Message, line, column);
                }
            }

            try
            {    
                return  new Token(TokenType.IntConstant, line, column, Convert.ToInt32(builder.ToString()));
            }
            catch(FormatException f)
            {
                throw new ScannerException(f.Message, line, column);
            }
        }
    }

    internal class SignedNumberLexer : NumberLexer
    {
        public override Token Scan(IReader reader)
        {
            char c = reader.Current;
            int line = reader.Line;
            int col = reader.Column;
            if (reader.MoveNext())
            {
                 if ('0' <= reader.Current && '9' >= reader.Current)
                {
                    var token = base.Scan(reader);
                    if (c == '+')
                    {
                        return new Token(token.TokenType, line, col, token.Text);
                    }

                    object obj = null;
                    switch(token.TokenType)
                    {
                        case TokenType.IntConstant: obj = (-1) * (int)token.Text; break;
                        case TokenType.DoubleConstant: obj = Convert.ToDouble((-1.0d) * (double)token.Text); break;
                        case TokenType.LongConstant: obj = Convert.ToInt64((-1) * (long)token.Text); break;
                        case TokenType.ShortConstant: obj = Convert.ToInt16((-1) * (short)token.Text); break;
                        case TokenType.FloatConstant: obj = (-1) * (float)token.Text; break;
			case TokenType.SByteConstant: obj = Convert.ToSByte( (-1) * (sbyte)token.Text); break;
                        default: throw new ScannerException($"Invalid {c} before {token.TokenType}", line, col);
                    }

                    return new Token(token.TokenType, line, col, obj);
                }

                reader.Revert(reader.Current);
            }

            return new Token(TokenType.Identifier, line, col, c.ToString());
        }
    }
}
