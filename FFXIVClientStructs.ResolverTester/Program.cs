using System.Diagnostics;
using System.Reflection.PortableExecutable;
using InteropGenerator.Runtime;

var gamePath = args.Length > 0 ? args[0] : @"C:\Games\FFXIV\game\ffxiv_dx11.exe";

using PEReader reader = new PEReader(File.OpenRead(gamePath));
SectionHeader textHeader = reader.PEHeaders.SectionHeaders[0];

Span<byte> relocFile = new Span<byte>(new byte[reader.PEHeaders.PEHeader!.SizeOfImage]);

reader.GetSectionData(textHeader.Name).GetContent().CopyTo(relocFile.Slice(textHeader.VirtualAddress, textHeader.VirtualSize));
unsafe {
    fixed (byte* bytes = relocFile) {

        Resolver.GetInstance.Setup(new IntPtr(bytes),
            relocFile.Length,
            textHeader.VirtualAddress,
            textHeader.VirtualSize,
            "version_test",
            new FileInfo("test.json"));

        var watch = new Stopwatch();
        watch.Start();
        FFXIVClientStructs.Interop.Generated.Addresses.Register();
        Resolver.GetInstance.Resolve();
        watch.Stop();
        Console.WriteLine($"Resolved in {watch.ElapsedMilliseconds}ms");

        var totalSigCount = Resolver.GetInstance.Addresses.Count;
        var resolvedCount = Resolver.GetInstance.Addresses.Count(sig => sig.Value != 0);
        Console.WriteLine($"Resolved count: {resolvedCount} ({((float)resolvedCount / totalSigCount) * 100}%)");

        Console.WriteLine("\n=== Broken Signatures ===");
        var unresolvedSigs = Resolver.GetInstance.Addresses.Where(sig => sig.Value == 0);
        foreach (var sig in unresolvedSigs)
            Console.WriteLine($"[FAIL] {sig.Name}: {sig.String}");

        foreach (Address address in Resolver.GetInstance.Addresses)
            Console.WriteLine($"{address.Name} {address.Value - new IntPtr(bytes):X}");
    }
}
