package controllers

import models.{NewStudentItem, Student}
import play.api.libs.json.{JsValue, Json}
import play.api.mvc.{Action, AnyContent, BaseController, ControllerComponents}
import javax.inject._
import scala.collection.mutable

/**
 * This controller creates an `Action` to handle HTTP requests to the
 * application's home page.
 */
@Singleton
class HomeController @Inject()(val controllerComponents: ControllerComponents) extends BaseController {

  /**
   * Create an Action to render an HTML page.
   *
   * The configuration in the `routes` file means that this method
   * will be called when the application receives a `GET` request with
   * a path of `/`.
   */

  val studentList = new mutable.ListBuffer[Student]()
  studentList += Student(1,"Delhi","Mayank")
  studentList += Student(2,"Mathura","Yashika")

    implicit val studentListJson = Json.format[Student]
    implicit val newStudentItem = Json.format[NewStudentItem]

  def index: Action[AnyContent] = Action { implicit request =>
   Ok(views.html.index("Meenakshi"))
  }

  def getAll(): Action[AnyContent] = Action {
    if (studentList.isEmpty) NoContent else Ok(Json.toJson(studentList))
  }

  def getById(studentId:Int) : Action[AnyContent] = Action {
    val stdId = studentList.find(_.id == studentId)
    stdId match {
      case Some(value) => Ok(Json.toJson(value))
      case None => NotFound
    }
  }

  def markAsDone(itemId: Long) = Action {
    val foundItem = studentList.find(_.id == itemId)
    foundItem match {
      case Some(item) =>
        val newItem = item.copy(address = "Kolkata")
        studentList.dropWhileInPlace(_.id == itemId)
        studentList += newItem
        Accepted(Json.toJson(newItem))
      case None => NotFound
    }
  }

  def deleteAllDone(id:Int) = Action {
    studentList.filterInPlace(_.id == id)
    Accepted
  }

  def addNewItem(): Action[JsValue] = Action(parse.json) { implicit request =>
    request.body.validate[NewStudentItem].asOpt
      .fold {
        BadRequest("No item added")
      } {
        response =>
                    val nextId = studentList.map(_.id).max + 1
                    val newItemAdded = Student(nextId, response.address, response.name)
                    studentList += newItemAdded
                    Ok(Json.toJson(studentList))
                  }
  }
  }
