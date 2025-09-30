'use client'

import { useState, useEffect } from 'react'
import { useRouter } from 'next/navigation'
import { BuildingOfficeIcon } from '@heroicons/react/24/outline'
import Input from '@/components/UI/Input'
import Button from '@/components/UI/Button'
import { Card } from '@/components/UI/Card'

export default function LoginPage() {
  const [username, setUsername] = useState('')
  const [password, setPassword] = useState('')
  const [rememberMe, setRememberMe] = useState(false)
  const [error, setError] = useState('')
  const [loading, setLoading] = useState(false)
  const router = useRouter()

  useEffect(() => {
    // Check if there's a saved username on component mount
    const savedUsername = localStorage.getItem('rememberedUsername')
    if (savedUsername) {
      setUsername(savedUsername)
      setRememberMe(true)
    }
    
    // Check if there's a valid persistent session
    const persistentSession = localStorage.getItem('persistentSession')
    if (persistentSession) {
      try {
        const sessionData = JSON.parse(persistentSession)
        const sessionExpiry = new Date(sessionData.expiry)
        
        if (sessionExpiry > new Date()) {
          // Session is still valid, restore user data and redirect
          localStorage.setItem('user', JSON.stringify(sessionData.userData))
          document.cookie = `user=${JSON.stringify(sessionData.userData)}; path=/`
          window.location.href = '/'
        } else {
          // Session expired, clear it
          localStorage.removeItem('persistentSession')
        }
      } catch (err) {
        // Invalid session data, clear it
        localStorage.removeItem('persistentSession')
      }
    }
  }, [])

  const handleSubmit = async (e: React.FormEvent) => {
    e.preventDefault()
    setError('')
    setLoading(true)

    try {
      // For demo purposes, accept admin/admin
      if (username === 'admin' && password === 'admin') {
        // Store user data in localStorage
        const userData = {
          name: 'System Administrator',
          email: 'admin@acas.local',
          role: 'Administrator',
          token: 'demo-token'
        }
        localStorage.setItem('user', JSON.stringify(userData))
        
        // Handle Remember Me
        if (rememberMe) {
          // Save username for next time
          localStorage.setItem('rememberedUsername', username)
          
          // Create persistent session (valid for 30 days)
          const expiryDate = new Date()
          expiryDate.setDate(expiryDate.getDate() + 30)
          
          const persistentSessionData = {
            userData,
            expiry: expiryDate.toISOString(),
            createdAt: new Date().toISOString()
          }
          localStorage.setItem('persistentSession', JSON.stringify(persistentSessionData))
          
          // Set a long-lived cookie
          document.cookie = `user=${JSON.stringify(userData)}; path=/; expires=${expiryDate.toUTCString()}`
        } else {
          // Clear any saved username if Remember Me is unchecked
          localStorage.removeItem('rememberedUsername')
          localStorage.removeItem('persistentSession')
          
          // Set session cookie (expires when browser closes)
          document.cookie = `user=${JSON.stringify(userData)}; path=/`
        }
        
        // Force page reload to ensure middleware picks up the authentication
        window.location.href = '/'
      } else {
        setError('Invalid username or password')
      }
    } catch (err) {
      setError('An error occurred. Please try again.')
    } finally {
      setLoading(false)
    }
  }

  return (
    <div className="min-h-screen bg-gray-50 flex flex-col justify-center py-12 sm:px-6 lg:px-8">
      <div className="sm:mx-auto sm:w-full sm:max-w-md">
        <div className="flex justify-center">
          <BuildingOfficeIcon className="h-12 w-12 text-indigo-600" />
        </div>
        <h2 className="mt-6 text-center text-3xl font-extrabold text-gray-900">
          Sign in to ACAS
        </h2>
        <p className="mt-2 text-center text-sm text-gray-600">
          Applewood Computers Accounting System
        </p>
      </div>

      <div className="mt-8 sm:mx-auto sm:w-full sm:max-w-md">
        <Card className="py-8 px-4 sm:px-10">
          <form className="space-y-6" onSubmit={handleSubmit}>
            {error && (
              <div className="rounded-md bg-red-50 p-4">
                <p className="text-sm text-red-800">{error}</p>
              </div>
            )}

            <Input
              label="Username"
              type="text"
              value={username}
              onChange={(e) => setUsername(e.target.value)}
              required
              autoComplete="username"
              placeholder="Enter your username"
            />

            <Input
              label="Password"
              type="password"
              value={password}
              onChange={(e) => setPassword(e.target.value)}
              required
              autoComplete="current-password"
              placeholder="Enter your password"
            />

            <div className="flex items-center">
              <label className="flex items-center">
                <input
                  type="checkbox"
                  className="h-4 w-4 text-indigo-600 focus:ring-indigo-500 border-gray-300 rounded"
                  checked={rememberMe}
                  onChange={(e) => setRememberMe(e.target.checked)}
                />
                <span className="ml-2 block text-sm text-gray-900">
                  Remember me for 30 days
                </span>
              </label>
            </div>

            <Button
              type="submit"
              className="w-full"
              disabled={loading}
            >
              {loading ? 'Signing in...' : 'Sign in'}
            </Button>
          </form>
        </Card>
      </div>
    </div>
  )
}