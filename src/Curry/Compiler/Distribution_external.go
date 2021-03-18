package CurryCompilerDistribution

import "gocurry"

func ExternalCurry_Compiler_Distribution_curryCompiler( task *gocurry.Task )(  ){
    gocurry.StringCreate( task.GetControl(  ), "curry2go" )
}

func ExternalCurry_Compiler_Distribution_curryCompilerMajorVersion( task *gocurry.Task )(  ){
    gocurry.IntLitCreate( task.GetControl(  ), 1 )
}

func ExternalCurry_Compiler_Distribution_curryCompilerMinorVersion( task *gocurry.Task )(  ){
    gocurry.IntLitCreate( task.GetControl(  ), 0 )
}

func ExternalCurry_Compiler_Distribution_curryCompilerRevisionVersion( task *gocurry.Task )(  ){
    gocurry.IntLitCreate( task.GetControl(  ), 0 )
}

func ExternalCurry_Compiler_Distribution_curryRuntime( task *gocurry.Task )(  ){
    gocurry.StringCreate( task.GetControl(  ), "go" )
}

func ExternalCurry_Compiler_Distribution_curryRuntimeMajorVersion( task *gocurry.Task )(  ){
    gocurry.GoMajVer( task.GetControl(  ) )
}

func ExternalCurry_Compiler_Distribution_curryRuntimeMinorVersion( task *gocurry.Task )(  ){
    gocurry.GoMinVer( task.GetControl(  ) )
}

func ExternalCurry_Compiler_Distribution_baseVersion( task *gocurry.Task )(  ){
    gocurry.StringCreate( task.GetControl(  ), "3.0.0" )
}

func ExternalCurry_Compiler_Distribution_installDir( task *gocurry.Task )(  ){
    gocurry.StringCreate( task.GetControl(  ), "/home/mh/curry2go/" )
}

