with AAA.Strings;
with CLIC.Subcommand;
with CLIC.TTY;

package Commands.Topics.License is

    package TT renames CLIC.TTY;

    type Topic is new CLIC.Subcommand.Help_Topic with null record;

    overriding function Name (This : Topic) return CLIC.Subcommand.Identifier is
      ("license");

    overriding function Title (This : Topic) return String is ("License info.");

    overriding function Content (This : Topic) return AAA.Strings.Vector is
      (AAA.Strings.Empty_Vector.New_Line.Append (TT.Bold ("DummyServer"))
         .New_Line
         .Append
         ("DummyServer is licensed under the Creative Commons Zero v1.0 Universal")
         .New_Line
         .Append
         ("The Creative Commons CC0 Public Domain Dedication waives copyright interest in a work you've created and dedicates it to the world-wide public domain. Use CC0 to opt out of copyright entirely and ensure your work has the widest reach. As with the Unlicense and typical software licenses, CC0 disclaims warranties. CC0 is very similar to the Unlicense.")
         .New_Line
         .Append
         ("The full license is available in the root folder of DummyServer and also online at " &
          TT.URL ("https://github.com/bracke/dummyserver/blob/main/LICENSE"))
         .New_Line
         .New_Line
         .Append (TT.Bold ("Black"))
         .New_Line
         .Append
         ("Black is a library that DummyServer uses. It was programmed by Jacob Sparre Andersen and it is available at " &
          TT.URL ("https://github.com/sparre/Black"))
         .Append ("Black is licensed under the following license: ")
         .New_Line
         .Append ("Black HTTP and Websocket library - License")
         .New_Line
         .Append ("Copyright (c) 2014, AdaHeads K/S")
         .Append ("Copyright (c) 2016, JSA Research & Innovation")
         .New_Line
         .Append
         ("Permission to use, copy, modify, and distribute this software for any purpose with or without fee is hereby granted, provided that the above copyright notice and this permission notice appear in all copies.")
         .New_Line
         .Append
         ("THE SOFTWARE IS PROVIDED 'AS IS' AND THE AUTHOR DISCLAIMS ALL WARRANTIES WITH REGARD TO THIS SOFTWARE INCLUDING ALL IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS. IN NO EVENT SHALL THE AUTHOR BE LIABLE FOR ANY SPECIAL, DIRECT, INDIRECT, OR CONSEQUENTIAL DAMAGES OR ANY DAMAGES WHATSOEVER RESULTING FROM LOSS OF USE, DATA OR PROFITS, WHETHER IN AN ACTION OF CONTRACT, NEGLIGENCE OR OTHER TORTIOUS ACTION, ARISING OUT OF OR IN CONNECTION WITH THE USE OR PERFORMANCE OF THIS SOFTWARE.")
         .New_Line
         .New_Line
         .Append (TT.Bold ("json-ada"))
         .New_Line
         .Append
         ("json-ada is a library that DummyServer uses. It was programmed by onox and it is available at " & TT.URL ("https://github.com/onox/json-ada"))
         .Append ("json-ada is licensed under the Apache License 2.0")
         .New_Line
         .Append ("A permissive license whose main conditions require preservation of copyright and license notices. Contributors provide an express grant of patent rights. Licensed works, modifications, and larger works may be distributed under different terms and without source code.")
         .New_Line
         .Append
         ("The full license is available online at " &
          TT.URL ("https://github.com/onox/json-ada/blob/master/LICENSE"))
         .New_Line
         .New_Line
         .Append (TT.Bold ("base64"))
         .New_Line
         .Append
         ("base64 is a library that DummyServer uses. It was programmed by AntonMeep and it is available at " & TT.URL ("https://github.com/AntonMeep/base64"))
         .Append ("base64 is licensed under the ISC License")
         .New_Line
         .Append ("A permissive license lets people do anything with your code with proper attribution and without warranty. The ISC license is functionally equivalent to the BSD 2-Clause and MIT licenses, removing some language that is no longer necessary.")
         .New_Line
         .Append
         ("The full license is available online at " &
          TT.URL ("https://github.com/AntonMeep/base64/blob/master/LICENSE.txt"))
         .New_Line
         .New_Line
         .Append (TT.Bold ("clic"))
         .New_Line
         .Append
         ("clic is a library that DummyServer uses. It was programmed by the Alire project and it is available at " & TT.URL ("https://github.com/alire-project/clic"))
         .Append ("base64 is licensed under the MIT License")
         .New_Line
         .Append ("A short and simple permissive license with conditions only requiring preservation of copyright and license notices. Licensed works, modifications, and larger works may be distributed under different terms and without source code.")
         .New_Line
         .Append ("The full license is available online at " &
          TT.URL ("https://github.com/alire-project/clic/blob/main/LICENSE"))
         .New_Line
         .New_Line
         );

end Commands.Topics.License;