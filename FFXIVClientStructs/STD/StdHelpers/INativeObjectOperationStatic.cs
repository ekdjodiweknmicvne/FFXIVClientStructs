namespace FFXIVClientStructs.STD.StdHelpers;

/// <summary>
/// Static instructions for how to deal with a type.
/// </summary>
/// <typeparam name="T">The type.</typeparam>
public interface INativeObjectOperationStatic<T>
    where T : unmanaged{
    /// <summary>
    /// Gets a value indicating whether the type is has a default value.
    /// </summary>
    public abstract static bool HasDefault { get; }

    /// <summary>
    /// Gets a value indicating whether the type is disposable.
    /// </summary>
    /// <remarks>If <c>false</c>, then <see cref="IDisposable.Dispose"/> will be skipped.</remarks>
    public abstract static bool IsDisposable { get; }

    /// <summary>
    /// Whether the type is copiable.
    /// </summary>
    /// <remarks>If <c>false</c>, then any operation resulting in copies will fail.</remarks>
    public abstract static bool IsCopiable { get; }

    /// <summary>
    /// Whether the type is movable.
    /// </summary>
    /// <remarks>If <c>false</c>, then any operation resulting in move will fail.</remarks>
    public abstract static bool IsMovable { get; }

    /// <summary>
    /// Sets the given item to the default value.
    /// </summary>
    /// <param name="item">The item to default.</param>
    /// <exception cref="NotSupportedException">If <see cref="HasDefault"/> is <c>false</c>.</exception>
    public abstract static void SetDefault(ref T item);

    /// <summary>
    /// Disposes the given item, if <see cref="IsDisposable"/> is true.
    /// </summary>
    /// <param name="item">The item to dispose.</param>
    public abstract static void Dispose(ref T item);

    /// <summary>
    /// Copies the given item.
    /// </summary>
    /// <param name="source">The source.</param>
    /// <param name="target">The target.</param>
    /// <exception cref="NotSupportedException">If <see cref="IsCopiable"/> is <c>false</c>.</exception>
    public abstract static void Copy(in T source, out T target);

    /// <summary>
    /// Moves the given item.
    /// </summary>
    /// <param name="source">The source.</param>
    /// <param name="target">The target.</param>
    /// <exception cref="NotSupportedException">If <see cref="IsMovable"/> is <c>false</c>.</exception>
    public abstract static void Move(ref T source, out T target);

    /// <summary>
    /// Swaps the given items.
    /// </summary>
    /// <param name="item1">The first item.</param>
    /// <param name="item2">The second item.</param>
    /// <exception cref="NotSupportedException">If <see cref="IsMovable"/> is <c>false</c>.</exception>
    public abstract static void Swap(ref T item1, ref T item2);
}
