// Copyright: Hemanth Kapila (2016).
// See the LICENSE file in the project root for more information.

using System;
using System.Collections;
using System.Collections.Generic;
using System.IO;

namespace CShark.Lexer
{
    interface IReader : IEnumerator<char>
    {
        int Line { get; }
        int Column { get; }
        void Revert(char c);
    }

    class Reader : IReader
    {
        private readonly TextReader _reader;
        private readonly Stack<char> _stack;
        private readonly Stack<int> _colStack;
        private char _next;
        private int _line;
        private int _column;

        public Reader(TextReader reader)
        {
            _reader = reader;
            _stack = new Stack<char>();
            _colStack = new Stack<int>();
            _next = Char.MinValue;
            _line = 1;
            _column = 0;
        }

        private void IncrementLine()
        {
            _line++;
            _colStack.Push(_column);
            _column = 0;
        }

        public int Line
        {
            get
            {
                return _line;
            }
        }

        public int Column
        {
            get
            {
                return _column;
            }
        }
        public char Current
        {
            get
            {
                return _next;
            }
        }

        object IEnumerator.Current
        {
            get
            {
                return Current;
            }
        }

        public void Revert(char c)
        {
            _column--;
            if (c == '\n')
            {
                _line--;
                if (_colStack.Count > 0)
                {
                    _column = _colStack.Pop();
                }
            }

            _stack.Push(c);
        }

        public bool MoveNext()
        {
            if (_reader == null)
                return false;

            if (_stack.Count > 0)
            {
                _next = _stack.Pop();
                _column++;
                return true;
            }
            int i = _reader.Read();
            if (i < 0)
            {
                return false;
            }

            _next = Convert.ToChar(i);
            _column++;
            if (_next == '\n')
            {
                IncrementLine();
            }
            return true;
        }

        public void Reset()
        {
            throw new InvalidOperationException("Reset not supported");
        }

        public void Dispose()
        {
            if (_reader != null)
            {
                _reader.Dispose();
            }
        }
    }
}
