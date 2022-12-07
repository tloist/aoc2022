import scala.reflect.TypeTest
import File._

case class Shell(pwd: FilePath, root: Directory) {
    override def toString(): String = s"Shell $pwd"

    def cd(filename: String): Shell = 
        if (filename == "..") return copy(pwd = pwd.parent.getOrElse(pwd))
        val current = if filename.startsWith("/") then root else pwdAs[Directory]
        current(filename).map(f => copy(pwd = f.path)).getOrElse(sys.error(s"There is no file named '$filename' at ${}"))
        

    def adapt(command: CommandWithOutput[_]): Shell = command match
        case CommandWithOutput(ChangeDirectory(change), _) => cd(change)
        case CommandWithOutput(ListDirectoryContents, fileRefs) =>
            val refs: List[File] = fileRefs
            val files = refs.map(_.resolveRelativeTo(pwd))
            val dir = files.foldLeft(pwdAs[Directory]) { (dir, file) => dir.addFile(file)}
            val pathToTop = LazyList.unfold(pwd) { _.parent.map(p => (p, p))}
            val newRoot = pathToTop.foldLeft(dir) { (exchange, base) => 
                val curBase = rootPathAs[Directory](base)
                curBase.exchangeFile(exchange)
            }
            copy(pwd, newRoot)
        case unknown => sys.error(s"Unsupported command '${command.getClass()}' encountered")
    def adapt(commands: Seq[CommandWithOutput[_]]): Shell = commands.foldLeft(this) { _ adapt _ }

    def file: File = root(pwd) match
        case Right(file) => file
        case Left(errorMsg) => sys.error(errorMsg)
    
    def rootPathAs[F <: File](path: FilePath)(using validFiletype: TypeTest[File, F]): F = root(path) match
        case Left(errorMsg) => sys.error(errorMsg)
        case Right(file) => file match
            case validFiletype(valid) => valid
            case _ => sys.error(s"Current working directory doesn't point to the expected type!")
    def pwdAs[F <: File](using validFiletype: TypeTest[File, F]): F = rootPathAs(pwd)

    def totalUsedSize: BigInt = root.size
    def totalFreeSize: BigInt = BigInt("70000000") - totalUsedSize

    def allDirectories(pred: Directory => Boolean): List[Directory] = root.filesRecursively.collect {
        case dir: Directory if pred(dir) => dir
    }
}

object Shell {
    def newSystem: Shell = 
        Shell(File.rootPath, Directory(File.rootPath, List.empty))

    def fromRoot(dir: Directory): Shell =
        require(dir.path.isRoot)
        Shell(dir.path, dir)
}