import File._

sealed trait File {
    def path: FilePath
    def size: BigInt
    def fullPath: String = path.fullPath  // TODO: Can I use export path.fullPath instead here?
    def filename = path.filename    // TODO: Can I use export path.filename instead here?
    def resolveRelativeTo(pwd: FilePath): File
}

object File {
    opaque type FilePath = List[String]

    val sep: String = "/"
    val pwd: String = "."
    def parsePath(input: String): Either[String, FilePath] = Right(input.split(sep).toList)
    def expectPath(input: String): FilePath = parsePath(input).getOrElse(sys.error(s"Expected '$input' to be a valid file path but it was not!"))
    def validate(path: FilePath): Boolean = path.forall(!_.contains(sep))
    def requireValid(path: FilePath) =
        require(validate(path), s"File path $path contain a part with a separate in it, making it illegal!")
    val rootPath: FilePath = List.empty
    def relativePath(name: String): FilePath = List(".", name)

    extension (path: FilePath)
        def isRoot: Boolean = path.isEmpty
        def filename: String = path.last
        def fullPath: String = 
            val joined = path.mkString(File.sep)
            if joined.startsWith(File.pwd) then joined else s"/$joined"
        def isSuccessorOf(other: FilePath): Boolean = path.zip(other).forall(_ == _) && path != other
        def isParentOf(other: FilePath): Boolean = isSuccessorOf(other) && path.size + 1 == other.size
        def isDescendantOf(other: FilePath): Boolean = other isSuccessorOf path
        def isChildOf(other: FilePath): Boolean = other isParentOf path
        def child(name: String): FilePath = path :+ name
        def parent: Option[FilePath] = Option(path.dropRight(1)).filter(_ != path)
        def level: Int = path.size
        def parts: List[String] = path
        def resolveRelativeTo(origin: FilePath): FilePath = path.flatMap(p => if p == pwd then origin else List(p))
}

case class DataFile(path: FilePath, size: BigInt) extends File {
    File.requireValid(path)
    override def toString(): String = s"${path.fullPath} ($size)"
    def resolveRelativeTo(pwd: FilePath): File = copy(path = path.resolveRelativeTo(pwd))
}
case class Directory(path: FilePath, files: List[File]) extends File {
    File.requireValid(path)
    override def toString(): String = s"${path.fullPath} (dir)"
    def size = filesRecursively.collect { case DataFile(_, size) => size }.sum
    def resolveRelativeTo(pwd: FilePath): File = copy(path = path.resolveRelativeTo(pwd))

    def apply(filename: String): Either[String, File] = File.parsePath(filename).flatMap(apply)
    def apply(path: FilePath): Either[String, File] =
        path.parts.foldLeft(Right(this): Either[String, File]) { (state, currentPath) => state.flatMap {
            case Directory(dirName, files) => 
                files.find(_.filename == currentPath) match
                    case Some(file) => Right(file)
                    case None => Left(s"Directory '$dirName' does not contain a file named '$currentPath'")
            case other => Left(s"Trying to access path '$currentPath' on something other than a directory: ${other.getClass()}")
        }
    }

    def mkdir(name: String): Directory =
        require(!name.contains(File.sep), s"'$name' is not a simple directory name")
        require(!files.exists(_.filename == name), s"Can't create an empty directory within path $name! There is already a file named like that!")
        copy(files = this.files :+ Directory(path.child(name), List.empty))

    def addFile(file: File): Directory =
        require(file.path isChildOf path, s"File ${filename} is not within this directory $fullPath")
        require(!files.contains(file.filename), s"Directory $fullPath already contains a file named ${file.filename}")
        copy(files = this.files :+ file)
    def addFile(name: String, size: BigInt): Directory = addFile(DataFile(path.child(name), size))
    def exchangeFile(file: File): Directory =
        require(file.path isChildOf path, s"File ${filename} is not within this directory $fullPath")
        require(files.exists(_.filename == file.filename), s"Directory ${file.fullPath} does not contains a file named ${file.filename}")
        copy(files = this.files.filterNot(_.filename == file.filename) :+ file)

    def filesRecursively: List[File] =
        def allFilesRec(left: List[File], visited: Set[FilePath], result: List[File]): List[File] = left match
            case Nil => result
            case head :: tail =>
                if (visited contains head.path) then allFilesRec(tail, visited, result)
                else head match
                    case _: DataFile => allFilesRec(tail, visited + head.path, result :+ head)
                    case dir: Directory => allFilesRec(dir.files ++ tail, visited + head.path, result :+ head)
        allFilesRec(List(this), Set.empty, List.empty)

    def tree: String = 
        (this +: filesRecursively).map { file =>
            val filename = file.filename.headOption.getOrElse("/")
            val indent = (0 until file.path.level).map(_ => "  ").mkString
            file match
                case DataFile(path, size) => s"$indent- $filename (file, size=$size)"
                case Directory(path, _) => s"$indent- $filename (dir)"
        }.mkString("\n")
}

object Directory {
    def root(files: List[File]): Directory = Directory(File.rootPath, files)
}
