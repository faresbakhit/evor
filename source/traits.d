template isType(T)
{
    template isType(U)
    {
        static if (is(T == U))
        {
            enum isType = true;
        }
        else
        {
            enum isType = false;
        }
    }
}
