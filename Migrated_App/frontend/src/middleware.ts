import { NextResponse } from 'next/server'
import type { NextRequest } from 'next/server'

export function middleware(request: NextRequest) {
  const pathname = request.nextUrl.pathname
  
  // Allow access to login page and public assets
  if (pathname.startsWith('/login') || 
      pathname.startsWith('/_next') || 
      pathname.startsWith('/favicon')) {
    return NextResponse.next()
  }
  
  // Get authentication from header (set by client)
  const authHeader = request.headers.get('x-auth-token')
  
  // For development, also check localStorage via cookie
  // In production, this would verify a JWT token from the backend
  const response = NextResponse.next()
  
  // If no auth, redirect to login
  if (!authHeader && pathname !== '/login') {
    // Check if we have user data stored
    const userCookie = request.cookies.get('user')
    if (!userCookie) {
      return NextResponse.redirect(new URL('/login', request.url))
    }
  }
  
  return response
}

export const config = {
  matcher: [
    /*
     * Match all request paths except for the ones starting with:
     * - api (API routes)
     * - _next/static (static files)
     * - _next/image (image optimization files)
     * - favicon.ico (favicon file)
     * - login
     */
    '/((?!api|_next/static|_next/image|favicon.ico|login).*)',
  ],
}