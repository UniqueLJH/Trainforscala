/**
 *
 * Created by lijiahang on 16/12/7.
 */
case class TreeNode[T](data: T, children: Seq[TreeNode[T]] = Nil)

object ShowTree{
  def asciiNode(root: TreeNode[String], prefix: String): Seq[String] = {
    var TreeString = Seq.empty[String]
    TreeString:+=("+-" + root.data);
    for (x <- root.children) {
      if (x != root.children.last && x.children.length > 0 ) {
        for (i <- asciiNode(x, "| ")) {
          TreeString:+=(prefix + i)
        }
      }else {
        for (i <- asciiNode(x, "  ")) {
          TreeString:+=(prefix + i)
        }
      }
    }
    if (prefix == "| ") {
      TreeString:+=("| ")
    }
    TreeString
  }
  def asciiDisplay(root: TreeNode[String]): Seq[String] = {
    val rtnString = asciiNode(root, "  ")
    rtnString
  }
   def main(args: Array[String]): Unit = {
     asciiDisplay(TreeNode("Root",
       children = List(
         TreeNode("level1-1", children = TreeNode("level2-1", children = TreeNode("level3-1") :: Nil) :: Nil),
         TreeNode("level1-2"),
         TreeNode("level1-3")))).foreach(println)
   }
}

