package co.uproot.abandon

object Main extends App {
  if(args.headOption.getOrElse("") equals "-g") {
    AbandonUI.main(args.tail)
  } else {
    AbandonApp.main(args)
  }
}
