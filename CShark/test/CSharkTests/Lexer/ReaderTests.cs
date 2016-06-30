// Copyright: Hemanth Kapila (2016).
// See the LICENSE file in the project root for more information.

using System;
using System.IO;
using System.Text;

using CShark.Lexer;
using Xunit;
namespace CSharkTests.Lexer
{
    public class ReaderTests : IDisposable
    {
        const string testFileName = "testInput.csk";

        [Fact]
        public void StringReaderTest()
        {
            var inputStr = "Hello CShark Reader!";
            var output = new StringBuilder();
            using (IReader reader = new Reader(new StringReader(inputStr)))
            {
                while (reader.MoveNext())
                {
                    output.Append(reader.Current);
                }
                Assert.Equal(1, reader.Line);
                Assert.Equal(inputStr.Length, reader.Column);
            }

            Assert.Equal(inputStr, output.ToString());
        }

        [Fact]
        public void FileReaderTest()
        {
            var inputStr = "Hello CShark Reader!";
            File.WriteAllText(testFileName, inputStr);
            var output = new StringBuilder();
            using (IReader reader = new Reader(new StreamReader(File.OpenRead(testFileName))))
            {
                while(reader.MoveNext())
                {
                    output.Append(reader.Current);
                }
                Assert.Equal(1, reader.Line);
                Assert.Equal(inputStr.Length, reader.Column);
            }

            Assert.Equal(inputStr, output.ToString());
        }

        [Fact]
        public void MultiLineFileTest()
        {
            var inputStr = "Hello\nCShark\nReader\n!!!";
            File.WriteAllText(testFileName, inputStr);
            var output = new StringBuilder();
            using (IReader reader = new Reader(new StreamReader(File.OpenRead(testFileName))))
            {
                while(reader.MoveNext())
                {
                    output.Append(reader.Current);
                }
                Assert.Equal(4, reader.Line);
                Assert.Equal(3, reader.Column);
            }

            Assert.Equal(inputStr, output.ToString());
        }

        [Fact]
        public void StringReader_Revert_Test()
        {
            var inputStr = "Hello CShark Reader!";
            var output = new StringBuilder();
            bool prevA = false;
            using (IReader reader = new Reader(new StringReader(inputStr)))
            {
                while (reader.MoveNext())
                {
                    output.Append(reader.Current);
                    if (reader.Current == 'a' && !prevA)
                    {
                        reader.Revert(reader.Current);
                        prevA = true;
                    }
                    else
                    {
                        prevA = false;
                    }
                }

                Assert.Equal(1, reader.Line);
                Assert.Equal(inputStr.Length, reader.Column);
            }

            Assert.Equal(inputStr.Replace("a", "aa"), output.ToString());
        }

        [Fact]
        public void File_MultiLine_Revert_Test()
        {
            var inputStr = "Hello\nCShark\nReader\n!!!";
            File.WriteAllText(testFileName, inputStr);
            var output = new StringBuilder();
            using (IReader reader = new Reader(new StreamReader(File.OpenRead(testFileName))))
            {
                int count = 0;
                while(reader.MoveNext())
                {
                    count++;
                    output.Append(reader.Current);
                }
                Assert.Equal(4, reader.Line);
                Assert.Equal(3, reader.Column);
                var tempString = output.ToString();

                for (int i=tempString.Length; i > 0; i--)
                {
                    reader.Revert(tempString[i-1]);
                }
                output.Length = 0;
                while(reader.MoveNext())
                {
                    output.Append(reader.Current);
                }
            }

            Assert.Equal(inputStr, output.ToString());
        }

        public void Dispose()
        {
            if (File.Exists(testFileName))
            {
                File.Delete(testFileName);
            }
        }
    }
}
