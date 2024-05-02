package ai.qwiet.springbootkotlinwebgoat

import org.springframework.http.HttpHeaders
import org.springframework.http.HttpStatusCode
import org.springframework.http.ResponseEntity
import org.springframework.web.ErrorResponse
import org.springframework.web.bind.annotation.ControllerAdvice
import org.springframework.web.context.request.WebRequest
import org.springframework.web.servlet.mvc.method.annotation.ResponseEntityExceptionHandler
import java.lang.Exception

@ControllerAdvice
class CustomRestExceptionHandler : ResponseEntityExceptionHandler() {
    override fun handleExceptionInternal(
        ex: Exception,
        body: Any?,
        headers: HttpHeaders,
        statusCode: HttpStatusCode,
        request: WebRequest
    ): ResponseEntity<Any>? {
        val errResponse = ErrorResponse.create(ex, statusCode, ex.message.orEmpty())
        // vulnerability: Exception Data Leaked in HTTP Response
        return ResponseEntity(errResponse, statusCode)
    }
}
