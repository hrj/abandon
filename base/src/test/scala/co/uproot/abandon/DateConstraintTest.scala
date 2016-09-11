package co.uproot.abandon

import java.time.{LocalDate, Month}

import org.scalamock.scalatest.MockFactory
import org.scalatest._

class DateConstraintTest extends FlatSpec with Matchers with BeforeAndAfterEach with MockFactory with OneInstancePerTest {

  class MockableDetailedPost extends DetailedPost(new AccountName(Seq()), 0, None, None)

  var appState: AppState = null

  override def beforeEach() {
    setupMocks()
  }

  it should "return true on valid posts" in {
    val constraint = new DateRangeConstraint(
      Some(DateBound(Date(2013, 1, 1), inclusive = true)),
      Some(DateBound(Date(2013, 12, 31), inclusive = false))
    )

    constraint.check(appState) should be(true)
  }

  it should "throw exception on end date violation" in {
    val constraint = new DateRangeConstraint(
      Some(DateBound(Date(2013, 1, 1), inclusive = false)),
      Some(DateBound(Date(2013, 11, 1), inclusive = false))
    )
    an [ConstraintError] should be thrownBy constraint.check(appState)

    val constraintWithoutFrom = new DateRangeConstraint(None, Some(DateBound(Date(2013, 11, 1), inclusive = true)))
    an [ConstraintError] should be thrownBy constraintWithoutFrom.check(appState)
  }

  it should "throw exception on inclusive end date violation" in {
    val constraint = new DateRangeConstraint(
      Some(DateBound(Date(2013, 1, 1), inclusive = true)),
      Some(DateBound(Date(2013, 11, 30), inclusive = true))
    )
    an [ConstraintError] should be thrownBy constraint.check(appState)

    val constraintWithoutFrom = new DateRangeConstraint(None, Some(DateBound(Date(2013, 11, 30), inclusive = true)))
    an [ConstraintError] should be thrownBy constraintWithoutFrom.check(appState)
  }

  it should "throw exception on exclusive from-date violation" in {
    val constraint = new DateRangeConstraint(
      Some(DateBound(Date(2013, 1, 1), inclusive = false)),
      Some(DateBound(Date(2013, 12, 2), inclusive = false))
    )
    an [ConstraintError] should be thrownBy constraint.check(appState)

    val constraintWithoutFrom = new DateRangeConstraint(Some(DateBound(Date(2013, 1, 1), inclusive = false)), None)
    an [ConstraintError] should be thrownBy constraintWithoutFrom.check(appState)
  }

  it should "throw exception on exclusive to-date violation" in {
    val constraint = new DateRangeConstraint(
      Some(DateBound(Date(2013, 1, 1), inclusive = true)),
      Some(DateBound(Date(2013, 12, 1), inclusive = false))
    )
    an [ConstraintError] should be thrownBy constraint.check(appState)

    val constraintWithoutFrom = new DateRangeConstraint(None, Some(DateBound(Date(2013, 12, 1), inclusive = false)))
    an [ConstraintError] should be thrownBy constraintWithoutFrom.check(appState)
  }

  it should "throw exception on start date violation" in {
    val constraint = new DateRangeConstraint(
      Some(DateBound(Date(2013, 6, 1), inclusive = false)),
      Some(DateBound(Date(2013, 12, 1), inclusive = false))
    )
    an [ConstraintError] should be thrownBy constraint.check(appState)

    val constraintWithoutTo = new DateRangeConstraint(Some(DateBound(Date(2013, 11, 1), inclusive = false)), None)
    an [ConstraintError] should be thrownBy constraintWithoutTo.check(appState)
  }

  it should "return true on empty dates" in {
    val constraintWithoutTo = new DateRangeConstraint(None, None)
    constraintWithoutTo.check(appState) should be(true)
  }


  def setupMocks() {
    val accState = stub[AccountState]
    appState = new AppState(accState)

    val posts: Seq[DetailedPost] = (1 to 12).map(month => {
      val post = stub[MockableDetailedPost]
      (post.date _).when().returns(new Date(2013, month, 1))
      post
    })

    (accState.posts _).when().returns(posts)
  }
}
