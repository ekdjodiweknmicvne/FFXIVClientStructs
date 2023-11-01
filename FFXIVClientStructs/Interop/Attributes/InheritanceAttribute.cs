using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using System.Threading.Tasks;

namespace FFXIVClientStructs.Interop.Attributes;
[AttributeUsage(AttributeTargets.Struct, AllowMultiple = true)]
internal class InheritanceAttribute<T>(int offset = 0x0) : Attribute 
    where T : unmanaged {
    public int Offset { get; set; } = offset;
}
