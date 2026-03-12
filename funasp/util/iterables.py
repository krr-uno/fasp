from typing import Callable, Iterable


def map_none[T](
    func: Callable[[T], T | None],
    lst: Iterable[T],
) -> list[T] | None:
    """Applies `func` to each item in `lst`, returning a list of results.
    If all results are None, returns None.
    Otherwise, returns a list of non-None results.
    None results are replaced by the item itself.
    """
    all_none = True
    new_list = []
    for item in lst:
        result = func(item)
        if result is not None:
            new_list.append(result)
            all_none = False
        else:
            new_list.append(item)
    return new_list if not all_none else None
