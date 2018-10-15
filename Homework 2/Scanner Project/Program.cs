using System;
using System.IO;
using System.Collections.Generic;

/*  
    Scanner for C#

    Scans C# code and breaks up the source code into tokens.

    Author: Nathan Bunch
    Date: 10/1/2018
    Purpose: Project 2, Programming languages Course, Houghton College
 */

namespace Scanner_Project
{
    enum StateID
    {
        Start = 0,
        SawLetter = 1,
        SawSymbol = 2,
        SawSlash = 3,
        SawSingleQuote = 4,
        SawDoubleQuote = 5,
        SawAtSymbol = 6,
        SawAtQuote = 7,
        SawNumber = 8,
        SawSpace = 9,
        SawEscape = 10
    }
    //handle the @"" string
    //handle numbers
    //handle doubles

    enum TokenID
    {
        IDENTIFIER = 0,
        SYMBOL = 1,
        STRING = 3,
        NUMBER = 4,
        COMMENT = 5,
        CHAR = 6
    }

    class Token
    {
        string token = "";
        TokenID tokenID;
        
        public Token(TokenID tokenID, string token)
        {
            this.tokenID = tokenID;
            this.token = token;
        }

        override public string ToString()
        {
            return (tokenID.ToString(), token).ToString();
        }
    }

    class State
    {
        string data = "";
        StateID stateID;
        bool isComment = false;
        bool at = false;
        bool extraSwitch = false;
        public State(StateID stateID, string data, bool at = false, bool isComment = false, bool extra = false)
        {
            this.stateID = stateID;
            this.data = data;
            this.at = at;
            this.isComment = isComment;
            this.extraSwitch = extra;
        }

        public StateID name()
        {
            return stateID;
        }

        public bool Comment()
        {
            return isComment;
        }

        public bool At()
        {
            return at;
        }
        public string acc()
        {
            return data;
        }

        public bool Extra()
        {
            return extraSwitch;
        }

        override public string ToString()
        {
            return (stateID.ToString(), data).ToString();
        }
    }

    class Program
    {
        public static bool isLetter(char c)
        {
            var letters = "abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ";
            return letters.Contains(c);
        }
        public static bool isSymbol(char c)
        {
            var symbols = "~!#$%^&*<>,.?\\|;:+=()[]{}";
            return symbols.Contains(c);
        }
        public static bool isNumber(char c)
        {
            var numbers = "1234567890";
            return numbers.Contains(c);
        }
        public static bool isPeriod(char c)
        {
            return c == '.';
        }
        public static bool isAt(char c)
        {
            return c == '@';
        }
        public static bool isSingleQuote(char c)
        {
            return c == '\'';
        }
        public static bool isDoubleQuote(char c)
        {
            return c == '"';
        }
        public static bool isSlash(char c)
        {
            return c == '/';
        }
        public static bool isAsterisk(char c)
        {
            return c == '*';
        }
        public static bool isNewLine(char c)
        {
            return c == '\n';
        }
        public static bool isEscape(char c)
        {
            return c == '\\';
        }
        public static void engine(StreamReader sr, String fn)
        {
            State s0 = new State(StateID.Start, "");
            var s = s0;
            char c;
            (Token, State) ts;
            Token t;
            StreamWriter wr = new StreamWriter(fn + @"read_out.txt");
            var count = 0;

            while(!sr.EndOfStream)
            {
                c = (char) sr.Read();
                ts = prog(c, s);
                t = ts.Item1;
                s = ts.Item2;
                if(t != null)
                    wr.WriteLine(t);
                    count += 1;
                    //Console.WriteLine(t);
                //ts = prog('\0', s);
                //t = ts.Item1;
                //s = ts.Item2;
                //if(t != null)
                //    Console.WriteLine(t);
            }

            Console.WriteLine("Done. {0} tokens extracted.", count);

            wr.Close();
        }

        public static (Token, State) prog(char c, State s)
        {
            if(s.name() == StateID.Start)
            {
                if(isLetter(c))
                {
                    return (null, new State(StateID.SawLetter, c.ToString()));
                }
                else if(isSymbol(c))
                {
                    return (new Token(TokenID.SYMBOL, c.ToString()), new State(StateID.Start, ""));
                }
                else if(isAt(c))
                {
                    return (null, new State(StateID.SawAtSymbol, c.ToString()));
                }
                else if(isSingleQuote(c))
                {
                    return (null, new State(StateID.SawSingleQuote, c.ToString()));
                }
                else if(isDoubleQuote(c))
                {
                    return (null, new State(StateID.SawDoubleQuote, c.ToString(), false, true));
                }
                else if(isNumber(c))
                {
                    return (null, new State(StateID.SawNumber, c.ToString()));
                }
                else if(isSlash(c))
                {
                    return (null, new State(StateID.SawSlash, c.ToString()));
                }
                else if(isEscape(c))
                {
                    return (null, new State(StateID.SawEscape, c.ToString()));
                }
                else
                    return (null, s);
            }
            else if(s.name() == StateID.SawLetter)
            {
                if(isLetter(c))
                {
                    return (null, new State(StateID.SawLetter, s.acc() + c.ToString()));
                }
                else if(isSymbol(c))
                {
                    return (new Token(TokenID.IDENTIFIER, s.acc()), new State(StateID.SawSymbol, c.ToString()));
                }
                else
                    return (new Token(TokenID.IDENTIFIER, s.acc()), new State(StateID.Start, ""));
            }
            else if(s.name() == StateID.SawSymbol)
            {
                if(isLetter(c))
                {
                    return (new Token(TokenID.SYMBOL, s.acc()), new State(StateID.SawLetter, c.ToString()));
                }
                else if(isSymbol(c))
                {
                    return (null, new State(StateID.SawSymbol, s.acc() + c.ToString()));
                }
                else
                    return (new Token(TokenID.SYMBOL, s.acc()), new State(StateID.Start, ""));
            }
            else if(s.name() == StateID.SawAtSymbol)
            {
                if(isDoubleQuote(c) && !s.At())
                {
                    return (null, new State(StateID.SawDoubleQuote, s.acc() + c.ToString(), true));
                }
                else
                    return (new Token(TokenID.SYMBOL, s.acc()), new State(StateID.Start, ""));
            }
            else if(s.name() == StateID.SawDoubleQuote)
            {
                if(s.At() && !isDoubleQuote(c))
                {
                    return (null, new State(StateID.SawDoubleQuote, s.acc() + c.ToString()));
                }
                else if(s.name() == StateID.SawDoubleQuote && isDoubleQuote(c))
                {
                    return (new Token(TokenID.STRING, s.acc() + c.ToString()), new State(StateID.Start, ""));
                }
                else
                    return (null, new State(StateID.SawDoubleQuote, s.acc() + c.ToString()));
            }
            else if(s.name() == StateID.SawNumber)
            {
                if(isNumber(c) || isPeriod(c))
                {
                    return (null, new State(StateID.SawNumber, s.acc() + c.ToString()));
                }
                else
                    return (new Token(TokenID.NUMBER, s.acc()), new State(StateID.Start, ""));
            }
            else if(s.name() == StateID.SawSlash)
            {
                if(isAsterisk(c))
                {
                    if(!s.Extra())
                        return (null, new State(StateID.SawSlash, s.acc() + c.ToString(), true, true, true));
                    else
                        return (null, new State(StateID.SawSlash, s.acc() + c.ToString(), false, true, true));
                }
                else if(isSlash(c) && s.Comment() && s.Extra() && !s.At())
                {
                    return (new Token(TokenID.COMMENT, s.acc() + s.ToString()), new State(StateID.Start, ""));
                }
                else if(isSlash(c) && s.Comment() && !s.Extra())
                {
                    return (null, new State(StateID.SawSlash, s.acc() + c.ToString(), true, true));
                }
                else if(isNewLine(c) && s.Comment() && !s.Extra())
                {
                    return (new Token(TokenID.COMMENT, s.acc() + c.ToString()), new State(StateID.Start, ""));
                }
                else
                    return (null, new State(StateID.SawSlash, s.acc() + c.ToString(), true, true, s.Extra()));
                
            }
            else if(s.name() == StateID.SawEscape)
            {
                return (new Token(TokenID.SYMBOL, s.acc() + c.ToString()), new State(StateID.Start, ""));
            }
            else if(s.name() == StateID.SawSingleQuote)
            {
                if(isSingleQuote(c) && s.acc()[s.acc().Length-1] != '\\')
                {
                    return (new Token(TokenID.CHAR, s.acc() + c.ToString()), new State(StateID.Start, ""));
                }
                else if(isSingleQuote(c) && s.acc()[s.acc().Length-2] == '\\')
                {
                    return (new Token(TokenID.CHAR, s.acc() + c.ToString()), new State(StateID.Start, ""));
                }
                else
                    return (null, new State(StateID.SawSingleQuote, s.acc() + c.ToString()));
            }
            else
                return (null, null);
        }
        static void Main(string[] args)
        {
            string file = args[0];
            engine(new StreamReader(file), file);
        }
    }
}
