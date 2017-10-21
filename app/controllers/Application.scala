package controllers

import javax.inject.Inject

import play.api.mvc.InjectedController

class Application @Inject()(app: controllers.ed2017.Application) extends InjectedController {
  def index = app.index
}
