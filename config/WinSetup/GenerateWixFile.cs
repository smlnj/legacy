using System;
using System.Collections.Generic;
using System.IO;
using System.Text;

namespace WixFromDirectory
{
    class Program
    {
        static void Main(string[] args)
        {
            if (args.Length == 0)
            {
                Console.WriteLine("Provide the name of the directory to turn into a WiX fragment.");
                return;
            }

            DirectoryInfo directory = new DirectoryInfo(args[0]);
            TextWriter writer = new StreamWriter(args[0] + ".wxs", false);

            writer.WriteLine("<Wix xmlns='http://schemas.microsoft.com/wix/2003/01/wi'>");
            writer.WriteLine("<Fragment Id='DirectoryFragment'>");
            writer.WriteLine("<DirectoryRef Id='INSTALLDIR'>");
            EmitDirectories(writer, directory, directory);
            writer.WriteLine("</DirectoryRef>");
            EmitFeature(writer);
            writer.WriteLine("</Fragment>");
            writer.WriteLine("</Wix>");
            writer.Close();
        }

        static void EmitFeature(TextWriter writer)
        {
            writer.WriteLine("<Feature Id='SubFiles' Title='SubFiles' Level='1'>");
            foreach (string s in Components)
            {
                writer.WriteLine(" <ComponentRef Id='{0}'/>", s);
            }
            writer.WriteLine("</Feature>");
        }

        static List<string> Components = new List<string>();
        static int UniqueId = -1;
        static string GenerateUniqueId(string id)
        {
            UniqueId++;
            return NumToAlphaBase(UniqueId);
        }

        static char[] Alphabet = new char[] { 'a', 'b', 'c', 'd', 'e', 'f', 'g', 'h', 'i', 'j', 'k', 'l', 'm', 'n', 'o', 'p', 'q', 'r', 's', 't', 'u', 'v', 'w', 'x', 'y', 'z' };
        static string NumToAlphaBase(int i)
        {
            string id = "";

            do
            {
                int modulus = 0;
                i = Math.DivRem(i, 26, out modulus);
                id = Alphabet[modulus] + id;
            } while (i > 0);

            return id;
        }

        static void EmitDirectories(TextWriter writer, DirectoryInfo subDir, DirectoryInfo root)
        {
            writer.WriteLine("<Directory Id='{0}' Name='{0}' LongName='{1}'>", GenerateUniqueId(subDir.Name), subDir.Name);
            FileInfo[] files = subDir.GetFiles();
            if (files.Length > 0)
            {
                string componentId = GenerateUniqueId(subDir.FullName);
                Components.Add(componentId);
                writer.WriteLine("<Component Id='{0}' Guid='{1}' DiskId='1'>", componentId, Guid.NewGuid().ToString("D"));
                foreach (FileInfo file in files)
                {
                    writer.WriteLine("<File Id='{0}' Name='{0}' LongName='{1}' src='{2}'/>", GenerateUniqueId(file.Name), file.Name, file.FullName);
                }

                writer.WriteLine("</Component>");
            }

            foreach (DirectoryInfo di in subDir.GetDirectories())
            {
                EmitDirectories(writer, di, root);
            }
            
            writer.WriteLine("</Directory>");
        }
    }
}
