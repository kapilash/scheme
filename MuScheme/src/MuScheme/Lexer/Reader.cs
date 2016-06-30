using System;
using System.Collections;
using System.Collections.Generic;
using System.IO;

namespace MuScheme.Lexer
{
    interface IReader : IEnumerator<char>
    {
        int Line { get; }
        int Column { get; }
        void Revert(char c);
        void Revert(string s);
    }

    class Reader : IReader
    {
        private readonly TextReader _reader;
        private readonly Stack<char> _stack;
        private char _next;
        private int _line;
        private int _column;

        public Reader(TextReader reader)
        {
            _reader = reader;
            _stack = new Stack<char>();
            _next = Char.MinValue;
            _line = 1;
            _column = 0;
        }

        private void IncrementLine()
        {
            _line++;
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
            _stack.Push(c);
        }

        public void Revert(string s)
        {
            for(int i=s.Length-1; i>=0; i--)
            {
                _stack.Push(s[i]);
            }
        }

        public bool MoveNext()
        {
            if (_reader == null)
                return false;

            if (_stack.Count > 0)
            {
                _next = _stack.Pop();
                return true;
            }
            int i = _reader.Read();
            if (i < 0)
            {
                return false;
            }

            _next = Convert.ToChar(i);
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
